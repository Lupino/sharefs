{-# LANGUAGE OverloadedStrings #-}
module ShareFS.FS
  (
    FS (..)
  , SimpleHandle (..)
  , newReadHandle
  , newWriteHandle
  , newReadWriteHandle
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

import qualified Control.Concurrent.Lock    as L (Lock, new, with)
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
                  | WriteHandle (IORef LB.ByteString) L.Lock
                  | ReadWriteHandle (IORef LB.ByteString) L.Lock

newWriteHandle :: IO SimpleHandle
newWriteHandle = do
  l <- L.new
  h <- newIORef LB.empty
  return $ WriteHandle h l

newReadHandle :: LB.ByteString -> IO SimpleHandle
newReadHandle bs = ReadHandle <$> newIORef bs

newReadWriteHandle :: LB.ByteString -> IO SimpleHandle
newReadWriteHandle bs = do
  l <- L.new
  h <- newIORef bs
  return $ ReadWriteHandle h l

simpleRead :: SimpleHandle -> Int64 -> Int64 -> IO (Either Errno LB.ByteString)
simpleRead handle byteCount offset = do
  case handle of
    (WriteHandle _ _)     -> return $ Left eNOSYS
    (ReadWriteHandle h l) -> Right <$> L.with l (seek h)
    (ReadHandle h)        -> Right <$> seek h

  where seekContents = LB.take byteCount . LB.drop offset
        seek h = (atomicModifyIORef' h $ \v -> (v, seekContents v))

writeData :: LB.ByteString -> LB.ByteString -> Int64 -> LB.ByteString
writeData bs dat offset | olen < offset = LB.concat $ [bs, patch, dat]
                        | otherwise = LB.take offset bs `LB.append` dat `LB.append` LB.drop (offset + len) bs

  where olen = LB.length bs
        len = LB.length dat
        patch = LB.pack $ take (fromIntegral $ offset - olen) (cycle " ")

simpleWrite :: SimpleHandle -> LB.ByteString -> Int64 -> IO (Either Errno Int64)
simpleWrite handle dat offset = do
  case handle of
    (ReadHandle _)        -> return $ Left eNOSYS
    (ReadWriteHandle h l) -> Right <$> L.with l (write h)
    (WriteHandle h l)     -> Right <$> L.with l (write h)

  where writeData bs | olen < offset = LB.concat $ [bs, patch, dat]
                     | otherwise     = LB.concat $ [ LB.take offset bs
                                                   , dat
                                                   , LB.drop (offset + len) bs
                                                   ]

          where olen = LB.length bs
                len = LB.length dat
                patch = LB.pack $ take (fromIntegral $ offset - olen) (cycle " ")

        write h = atomicModifyIORef' h $ \v -> (writeData v, LB.length dat)

simpleRelease :: FS -> FilePath -> SimpleHandle -> IO ()
simpleRelease fs path handle =
  case handle of
    (ReadHandle _)        -> return ()
    (ReadWriteHandle h l) -> L.with l $ save h
    (WriteHandle h l)     -> L.with l $ save h

  where save h = void $ putFile fs path =<< atomicModifyIORef' h (\v -> (LB.empty, v))
