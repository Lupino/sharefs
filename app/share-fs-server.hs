{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad                        (forM, when)
import qualified Data.ByteString.Char8                as BC (unpack)
import qualified Data.ByteString.Lazy                 as LB (ByteString, empty,
                                                             readFile,
                                                             writeFile)
import           Data.Int                             (Int64)
import           Data.List                            (isPrefixOf)
import           Data.Maybe                           (catMaybes)
import qualified Data.Text                            as T (Text, pack, unpack)
import           Network.Mime                         (MimeType,
                                                       defaultMimeLookup)
import           System.Directory                     (createDirectoryIfMissing,
                                                       doesDirectoryExist,
                                                       doesFileExist,
                                                       getDirectoryContents,
                                                       removeDirectoryRecursive,
                                                       removeFile, renamePath)
import           System.FilePath                      (dropDrive, dropExtension,
                                                       dropFileName,
                                                       takeFileName, (<.>),
                                                       (</>))
import           Text.Read                            (readMaybe)

import           Data.Streaming.Network.Internal      (HostPreference (Host))
import qualified Data.Text.Lazy                       as LT (pack)
import           Network.HTTP.Types                   (status204, status404)
import           Network.Wai                          (Request (..))
import           Network.Wai.Handler.Warp             (setHost, setPort)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Web.Scotty                           (ActionM, RoutePattern,
                                                       ScottyM, body, delete,
                                                       function, get, json,
                                                       middleware, param, post,
                                                       put, raw, scottyOpts,
                                                       setHeader, settings,
                                                       status)

import           ShareFS.SimpleStat

import           Data.UnixTime

import           Data.Default.Class                   (def)

import           Control.Monad.IO.Class               (liftIO)
import           System.IO                            (IOMode (ReadMode),
                                                       hFileSize, withFile)

import           Data.Semigroup                       ((<>))
import           Options.Applicative

data Options = Options { getHost :: String
                       , getPort :: Int
                       , getPath :: FilePath
                       }

parser :: Parser Options
parser = Options <$> strOption (long "host"
                                <> short 'H'
                                <> metavar "HOST"
                                <> help "ShareFS server host."
                                <> value "127.0.0.1")

                 <*> option auto (long "port"
                                  <> short 'p'
                                  <> metavar "PORT"
                                  <> help "ShareFS server port."
                                  <> value 3000)

                 <*> strOption (long "path"
                                <> metavar "PATH"
                                <> help "ShareFS server directory."
                                <> value "cache")

main :: IO ()
main = execParser opts >>= program
  where
    opts = info (helper <*> parser)
      ( fullDesc
     <> progDesc "ShareFS server"
     <> header "share-fs-server - ShareFS server" )

program :: Options -> IO ()
program (Options {getHost = host, getPort = port, getPath = path }) = do
  scottyOpts opts $ application path

  where opts = def { settings = setPort port $ setHost (Host host) (settings def) }

application :: FilePath -> ScottyM ()
application root = do
  middleware logStdout
  get    (matchPath ["file"])   $ getFileHandler    root
  put    (matchPath ["file"])   $ putFileHandler    root
  delete (matchPath ["file"])   $ deleteFileHandler root
  get    (matchPath ["stat"])   $ fileStatHandler   root
  get    (matchPath ["dir"])    $ listDirHandler    root
  put    (matchPath ["dir"])    $ putDirHandler     root
  post   (matchPath ["rename"]) $ renameHandler     root

matchPath :: [T.Text] -> RoutePattern
matchPath strs = function $ \req ->
  if isPrefixOf strs (pathInfo req) then
    Just [("path", LT.pack $ foldr ((</>) . T.unpack) "" (drop 1 $ pathInfo req))]
  else Nothing

getFileHandler :: FilePath -> ActionM ()
getFileHandler root = do
  path <- filePath root
  setHeader "Content-Type" $ LT.pack $ BC.unpack $ getMimeType path
  fileExists <- liftIO $ doesFileExist path
  if fileExists then raw =<< liftIO (LB.readFile path)
                else status status404 >> raw LB.empty

putFileHandler :: FilePath -> ActionM ()
putFileHandler root = do
  path <- filePath root
  wb <- body
  liftIO $ saveFile path wb

  status status204
  raw LB.empty

deleteFileHandler :: FilePath -> ActionM ()
deleteFileHandler root = do
  path <- filePath root
  modePath <- fileModePath root
  typePath <- fileTypePath root
  timePath <- fileTimePath root
  liftIO $ do
    deleteFile path
    deleteFile modePath
    deleteFile typePath
    deleteFile timePath
    hasStatDir <- doesDirectoryExist (dropExtension modePath)
    when hasStatDir $ deleteFile (dropExtension modePath)

  status status204
  raw LB.empty

fileStatHandler :: FilePath -> ActionM ()
fileStatHandler root = do
  modePath <- fileModePath root
  typePath <- fileTypePath root
  timePath <- fileTimePath root
  path <- filePath root

  ret <- liftIO $ do
    hasMode <- doesFileExist modePath
    hasType <- doesFileExist typePath
    hasTime <- doesFileExist timePath
    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path
    size <- if isFile then getFileSize path
                      else return 0

    mode <- if hasMode then (readMaybe <$> (readFile modePath) :: IO (Maybe Int64))
                       else return Nothing

    tp <- if hasType then readFile typePath
                     else return "F"

    time <- if hasTime then (readMaybe <$> (readFile timePath) :: IO (Maybe (Int64, Int64)))
                       else return Nothing

    now <- read . show . toEpochTime <$> getUnixTime

    let m = case mode of
              Nothing -> 0
              Just v  -> v

        t = case time of
              Nothing -> (now, now)
              Just v  -> v

    case (isFile, isDir) of
      (False, True) -> return ("D", m, 0, fst t, snd t)
      (True, False) -> return (tp, m, size, fst t, snd t)
      _             -> return ("E", 0, 0, 0, 0)

  case ret of
    ('E':_, _, _, _, _) -> status status404 >> raw LB.empty
    (xs, m, s, mt, ct) -> json SimpleStat { simpleType = head $ xs ++ "E"
                                          , simpleName = takeFileName path
                                          , simpleMode = m
                                          , simpleSize = fromIntegral s
                                          , simpleMTime = mt
                                          , simpleCTime = ct
                                          }

listDirHandler :: FilePath -> ActionM ()
listDirHandler root = do
  path <- filePath root
  typePath <- dropExtension <$> fileTypePath root
  isDir <- liftIO $ doesDirectoryExist path
  if isDir then json =<< catMaybes <$> liftIO (getDirInfo path typePath)
           else status status404 >> raw LB.empty

putDirHandler :: FilePath -> ActionM ()
putDirHandler root = do
  path <- filePath root
  liftIO $ createDirectoryIfMissing True path

  status status204
  raw LB.empty

renameHandler :: FilePath -> ActionM ()
renameHandler root = do
  srcPath <- filePath root
  srcMode <- fileModePath root
  srcType <- fileTypePath root
  srcTime <- fileTimePath root
  dst <- dropDrive <$> param "dst"
  let dstPath = root </> dst
      dstMode = root </> ".fs" </> dst <.> "mode"
      dstType = root </> ".fs" </> dst <.> "type"
      dstTime = root </> ".fs" </> dst <.> "time"

  liftIO $ do
    renamePath srcPath dstPath
    hasMode <- doesFileExist srcMode
    when hasMode $ renamePath srcMode dstMode
    hasType <- doesFileExist srcType
    when hasType $ renamePath srcType dstType
    hasTime <- doesFileExist srcTime
    when hasTime $ renamePath srcTime dstTime
    hasStatDir <- doesDirectoryExist (dropExtension srcMode)
    when hasStatDir $ renamePath (dropExtension srcMode) (dropExtension dstMode)

  status status204
  raw LB.empty

getDirInfo :: FilePath -> FilePath -> IO [Maybe SimpleStat]
getDirInfo topdir typedir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", "..", ".fs"]) names

  forM properNames $ \name -> do
    isDirectory <- doesDirectoryExist (topdir </> name)
    isFile <- doesFileExist (topdir </> name)
    hasType <- doesFileExist (typedir </> name)

    size <- if isFile then getFileSize (topdir </> name)
                      else return 0

    tp <- if hasType then readFile (typedir </> name)
                     else return "F"

    case (isDirectory, isFile) of
      (True, False) -> return . Just $ simpleStat 'D' name
      (False, True) -> return . Just $ (simpleStat (head (tp ++ "E")) name) { simpleSize = fromIntegral size }
      _             -> return Nothing

filePath :: FilePath -> ActionM FilePath
filePath root = do
  path <- dropDrive <$> param "path"
  return $ root </> path

fileModePath :: FilePath -> ActionM FilePath
fileModePath root = do
  path <- dropDrive <$> param "path"
  return $ root </> ".fs" </> path <.> "mode"

fileTypePath :: FilePath -> ActionM FilePath
fileTypePath root = do
  path <- dropDrive <$> param "path"
  return $ root </> ".fs" </> path <.> "type"

fileTimePath :: FilePath -> ActionM FilePath
fileTimePath root = do
  path <- dropDrive <$> param "path"
  return $ root </> ".fs" </> path <.> "time"

saveFile :: FilePath -> LB.ByteString -> IO ()
saveFile fn fc = do
  createDirectoryIfMissing True dir
  LB.writeFile fn fc

  where dir = dropFileName fn

deleteFile :: FilePath -> IO ()
deleteFile fn = do
  isDirectory <- doesDirectoryExist fn
  when isDirectory $ removeDirectoryRecursive fn
  fileExists <- doesFileExist fn
  when fileExists $ removeFile fn

-- | Guess MIME type from file extension
getMimeType :: FilePath -> MimeType
getMimeType = defaultMimeLookup . T.pack

getFileSize :: FilePath -> IO Integer
getFileSize path = withFile path ReadMode hFileSize
