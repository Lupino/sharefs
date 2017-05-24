{-# LANGUAGE OverloadedStrings #-}

module ShareFS.Client
  (
    getFile
  , putFile
  , deleteFile
  , statFile
  , getDir
  , putDir
  , renameFile
  , Gateway (..)
  , initMgr
  ) where

import qualified Data.ByteString.Lazy as LB (ByteString, empty, toStrict)
import qualified Data.Text.Lazy       as LT (pack)
import           Network.Wreq
import           ShareFS.Internal     (Gateway (..), initMgr)
import           ShareFS.SimpleStat   (SimpleStat (..))
import           ShareFS.Wreq

-- get "/file/path/to/filename"
getFile :: FilePath -> Gateway -> IO (Either String LB.ByteString)
getFile path gw = do
  responseEither $ getWith (getOptions gw) uri

  where uri = getGWUri gw ++ "/file" ++ path

-- put "/file/path/to/filename"
putFile :: FilePath -> LB.ByteString -> Gateway -> IO (Either String ())
putFile path dat gw = do
  opts <- getOptionsAndSignRaw ("/file" ++ path) (LB.toStrict dat) gw
  responseEither' $ putWith opts uri dat

  where uri = getGWUri gw ++ "/file" ++ path

-- delete "/file/path/to/filename"
deleteFile :: FilePath -> Gateway -> IO (Either String ())
deleteFile path gw = do
  opts <- getOptionsAndSign [( "sign_path", LT.pack ("/file" ++ path) )] gw
  responseEither' $ deleteWith opts uri

  where uri = getGWUri gw ++ "/file" ++ path

-- get "/stat/path/to/filename_or_dir"
statFile :: FilePath -> Gateway -> IO (Either String SimpleStat)
statFile path gw = do
  responseEitherJSON $ getWith (getOptions gw) uri

  where uri = getGWUri gw ++ "/stat" ++ path

-- get "/dir/path/to/dir"
getDir :: FilePath -> Gateway -> IO (Either String [SimpleStat])
getDir path gw = do
  responseEitherJSON $ getWith (getOptions gw) uri

  where uri = getGWUri gw ++ "/dir" ++ path

-- put "/dir/path/to/dir"
putDir :: FilePath -> Gateway -> IO (Either String ())
putDir path gw = do
  opts <- getOptionsAndSign [( "sign_path", LT.pack ("/dir" ++ path) )] gw
  responseEither' $ putWith opts uri LB.empty

  where uri = getGWUri gw ++ "/dir" ++ path

-- post "/rename/path/to/dir"
renameFile :: FilePath -> FilePath -> Gateway -> IO (Either String ())
renameFile src dst gw = do
  opts <- getOptionsAndSign [( "sign_path", LT.pack ("/rename" ++ src) ), ("dst", LT.pack dst)] gw
  responseEither' $ postWith opts uri [ "dst" := dst ]

  where uri = getGWUri gw ++ "/rename" ++ src
