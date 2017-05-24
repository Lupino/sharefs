{-# LANGUAGE OverloadedStrings #-}
module ShareFS.FS
  (
    FS (..)
  , SimpleHandle (..)
  , newReadHandle
  , newWriteHandle
  , simpleRead
  , simpleWrite
  , simpleRelease
  ) where

import qualified Data.ByteString.Lazy.Char8 as LB (ByteString, append, concat,
                                                   drop, empty, length, pack,
                                                   take)

import           Control.Monad              (void)
import           Data.Int                   (Int64)
import           Foreign.C.Error
import           ShareFS.SimpleStat         (SimpleStat)

import           Data.IORef                 (IORef, atomicModifyIORef',
                                             newIORef)

data FS = FS { putFile    :: FilePath -> LB.ByteString -> IO Errno
             , getFile    :: FilePath -> IO (Either Errno LB.ByteString)
             , deleteFile :: FilePath -> IO Errno
             , getDir     :: FilePath -> IO (Either Errno [SimpleStat])
             , putDir     :: FilePath -> IO Errno
             , renameFile :: FilePath -> FilePath -> IO Errno
             , statFile   :: FilePath -> IO (Either Errno SimpleStat)
             }

data SimpleHandle = ReadHandle (IORef LB.ByteString)
                  | WriteHandle (IORef LB.ByteString)

newWriteHandle :: IO SimpleHandle
newWriteHandle = WriteHandle <$> newIORef LB.empty

newReadHandle :: LB.ByteString -> IO SimpleHandle
newReadHandle bs = ReadHandle <$> newIORef bs

simpleRead :: SimpleHandle -> Int64 -> Int64 -> IO (Either Errno LB.ByteString)
simpleRead (WriteHandle _) _ _ = return $ Left eACCES
simpleRead (ReadHandle h) byteCount offset =
  Right . seekContents <$> (atomicModifyIORef' h $ \v -> (v, v))

  where seekContents :: LB.ByteString -> LB.ByteString
        seekContents = LB.take byteCount . LB.drop offset

simpleWrite :: SimpleHandle -> LB.ByteString -> Int64 -> IO (Either Errno Int64)
simpleWrite (ReadHandle _) _ _  = return $ Left eACCES
simpleWrite (WriteHandle h) dat offset = do
  atomicModifyIORef' h $ \v -> (writeData v, ())
  return . Right $ LB.length dat

  where writeData :: LB.ByteString -> LB.ByteString
        writeData bs | olen < offset = LB.concat $ [bs, patch, dat]
                     | otherwise = LB.take offset bs `LB.append` dat `LB.append` LB.drop (offset + len) bs

          where olen = LB.length bs
                len = LB.length dat
                patch = LB.pack $ take (fromIntegral $ offset - olen) (cycle " ")

simpleRelease :: FS -> FilePath -> SimpleHandle -> IO ()
simpleRelease _ _ (ReadHandle _) = return ()
simpleRelease fs path (WriteHandle h) = do
  dat <- atomicModifyIORef' h $ \v -> (LB.empty, v)
  void $ putFile fs path dat
