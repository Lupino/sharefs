{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import qualified Data.ByteString.Lazy as LB (ByteString)

import           ShareFS              (FS (FS), SimpleStat (..), fuseOps)
import qualified ShareFS              as FS

import           System.Fuse          (Errno, defaultExceptionHandler, eACCES,
                                       eNOENT, eOK, fuseRun)

import           Data.Aeson           (FromJSON, parseJSON, withObject, (.!=),
                                       (.:), (.:?))

import           ShareFS.Client


import           Data.Semigroup       ((<>))
import qualified Data.Yaml            as Y
import           Options.Applicative


data Options = Options { getConfigFile  :: String }

parser :: Parser Options
parser = Options <$> strOption (long "config"
                                <> short 'c'
                                <> metavar "FILE"
                                <> help "fuse-fs config file."
                                <> value "config.yaml")

data Config = Config { fileCache :: Gateway, fuseOpt :: [String] }
  deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    fileCache <- o .: "filecache"
    fuseOpt   <- o .:? "fuse" .!= []
    return Config {..}

main :: IO ()
main = execParser opts >>= program
  where
    opts = info (helper <*> parser)
      ( fullDesc
      <> progDesc "ShareFS"
      <> header "share-fs - ShareFS" )

program :: Options -> IO ()
program Options { getConfigFile = configFile } = do
  c <- Y.decodeFile configFile :: IO (Maybe Config)
  case c of
    Nothing     -> putStrLn "Config file format error"
    Just config -> startFS config

startFS :: Config -> IO ()
startFS Config { fileCache = fc, fuseOpt = opt } = do
  gw <- initMgr fc
  st <- FS.newOpenedStore
  fuseRun "share-fs" opt (fuseOps (fs gw st)) defaultExceptionHandler

fs :: Gateway -> FS.OpenedStore -> FS
fs gw st = FS { FS.putFile     = fsPutFile gw
              , FS.getFile     = fsGetFile gw
              , FS.deleteFile  = fsDeleteFile gw
              , FS.getDir      = fsGetDir gw
              , FS.putDir      = fsPutDir gw
              , FS.renameFile  = fsRenameFile gw
              , FS.statFile    = fsStatFile gw
              , FS.openedStore = st
              }

fsPutFile :: Gateway -> FilePath -> LB.ByteString -> IO Errno
fsPutFile gw path bs = do
  ret <- putFile path bs gw
  case ret of
    Left _  -> return eACCES
    Right _ -> return eOK

fsGetFile :: Gateway -> FilePath -> IO (Either Errno LB.ByteString)
fsGetFile gw path = do
  ret <- getFile path gw
  case ret of
    Left _    -> return $ Left eNOENT
    Right dat -> return $ Right dat

fsDeleteFile :: Gateway -> FilePath -> IO Errno
fsDeleteFile gw path = do
  ret <- deleteFile path gw
  case ret of
    Left _  -> return eACCES
    Right _ -> return eOK

fsGetDir :: Gateway -> FilePath -> IO (Either Errno [SimpleStat])
fsGetDir gw path = do
  ret <- getDir path gw
  case ret of
    Left _    -> return $ Left eNOENT
    Right dat -> return $ Right dat

fsPutDir :: Gateway -> FilePath -> IO Errno
fsPutDir gw path = do
  ret <- putDir path gw
  case ret of
    Left _  -> return eACCES
    Right _ -> return eOK

fsRenameFile :: Gateway -> FilePath -> FilePath -> IO Errno
fsRenameFile gw src dst = do
  ret <- renameFile src dst gw
  case ret of
    Left _  -> return eACCES
    Right _ -> return eOK

fsStatFile :: Gateway -> FilePath -> IO (Either Errno SimpleStat)
fsStatFile gw path = do
  ret <- statFile path gw
  case ret of
    Left _     -> return $ Left eNOENT
    Right stat -> return $ Right stat
