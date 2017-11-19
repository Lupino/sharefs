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
  , newOpenedStore
  , addOpenedStore
  , removeOpenedStore
  , getOpenedHandle
  , OpenedStore
  ) where

import qualified Data.ByteString.Lazy.Char8 as LB (ByteString, concat, drop,
                                                   empty, length, pack, take)

import           Control.Monad              (void)
import           Data.Int                   (Int64)
import           Foreign.C.Error
import           ShareFS.SimpleStat         (SimpleStat)

import qualified Control.Concurrent.Lock    as L (Lock, new, with)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HM (delete, empty, insert,
                                                   lookup)
import           Data.IORef                 (IORef, atomicModifyIORef',
                                             newIORef)

type OpenedStore = IORef (HashMap FilePath SimpleHandle)

data FS = FS { putFile     :: FilePath -> LB.ByteString -> IO Errno
             , getFile     :: FilePath -> IO (Either Errno LB.ByteString)
             , deleteFile  :: FilePath -> IO Errno
             , getDir      :: FilePath -> IO (Either Errno [SimpleStat])
             , putDir      :: FilePath -> IO Errno
             , renameFile  :: FilePath -> FilePath -> IO Errno
             , statFile    :: FilePath -> IO (Either Errno SimpleStat)
             , openedStore :: OpenedStore
             }

newOpenedStore :: IO OpenedStore
newOpenedStore = newIORef HM.empty

addOpenedStore :: FS -> FilePath -> SimpleHandle -> IO ()
addOpenedStore fs path sh = atomicModifyIORef' h $ \v -> (HM.insert path sh v, ())
  where h = openedStore fs

removeOpenedStore :: FS -> FilePath -> IO ()
removeOpenedStore fs path = atomicModifyIORef' h $ \v -> (HM.delete path v, ())
  where h = openedStore fs

getOpenedHandle :: FS -> FilePath -> IO (Maybe SimpleHandle)
getOpenedHandle fs path = atomicModifyIORef' h $ \v -> (v, HM.lookup path v)
  where h = openedStore fs

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
simpleRead handle byteCount offset =
  case handle of
    (WriteHandle _ _)     -> return $ Left eNOSYS
    (ReadWriteHandle h l) -> Right <$> L.with l (seek h)
    (ReadHandle h)        -> Right <$> seek h

  where seekContents = LB.take byteCount . LB.drop offset
        seek h = atomicModifyIORef' h $ \v -> (v, seekContents v)

simpleWrite :: SimpleHandle -> LB.ByteString -> Int64 -> IO (Either Errno Int64)
simpleWrite handle dat offset =
  case handle of
    (ReadHandle _)        -> return $ Left eNOSYS
    (ReadWriteHandle h l) -> Right <$> L.with l (write h)
    (WriteHandle h l)     -> Right <$> L.with l (write h)

  where writeData bs | olen < offset = LB.concat [bs, patch, dat]
                     | otherwise     = LB.concat [ LB.take offset bs
                                                 , dat
                                                 , LB.drop (offset + len) bs
                                                 ]

          where olen = LB.length bs
                len = LB.length dat
                patch = LB.pack $ take (fromIntegral $ offset - olen) (cycle " ")

        write h = atomicModifyIORef' h $ \v -> (writeData v, LB.length dat)

simpleRelease :: FS -> FilePath -> SimpleHandle -> IO ()
simpleRelease fs path handle = do
  removeOpenedStore fs path
  case handle of
    (ReadHandle _)        -> return ()
    (ReadWriteHandle h l) -> L.with l $ save h
    (WriteHandle h l)     -> L.with l $ save h

  where save h = void $ putFile fs path =<< atomicModifyIORef' h (\v -> (LB.empty, v))
