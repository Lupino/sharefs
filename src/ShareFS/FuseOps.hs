{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module ShareFS.FuseOps
  (
    fuseOps
  ) where

import qualified Data.ByteString.Char8      as B (ByteString, pack)
import qualified Data.ByteString.Lazy.Char8 as LB (empty, fromStrict, pack,
                                                   toStrict, unpack)
import           System.Posix.Files         (groupExecuteMode, groupReadMode,
                                             groupWriteMode, otherExecuteMode,
                                             otherReadMode, otherWriteMode,
                                             ownerExecuteMode, ownerReadMode,
                                             ownerWriteMode)

import           System.Posix.Types         (ByteCount, DeviceID, FileMode,
                                             FileOffset)

import           Control.Monad              (void)
import           Data.Int                   (Int64)
import           Data.Maybe                 (catMaybes)
import           System.FilePath            ((<.>))
import           System.Fuse
import           Text.Read                  (readMaybe)

import           ShareFS.FS                 (FS (..), SimpleHandle (..),
                                             newReadHandle, newWriteHandle)
import qualified ShareFS.FS                 as FS
import           ShareFS.SimpleStat         (SimpleStat (..))

fuseOps :: FS -> FuseOperations SimpleHandle
fuseOps fs = defaultFuseOps
  { fuseGetFileStat        = simpleGetFileStat fs
  , fuseOpen               = simpleOpen fs
  , fuseRead               = simpleRead fs
  , fuseWrite              = simpleWrite fs
  , fuseOpenDirectory      = simpleOpenDirectory fs
  , fuseReadDirectory      = simpleReadDirectory fs
  , fuseCreateDirectory    = simpleCreateDirectory fs
  , fuseGetFileSystemStats = simpleGetFileSystemStats
  , fuseSetFileSize        = \ _ _ -> pure eOK
  , fuseSetFileMode        = simpleSetFileMode fs
  , fuseCreateDevice       = simpleCreateDevice fs
  , fuseRelease            = FS.simpleRelease fs
  , fuseRemoveLink         = simpleRemove fs
  , fuseRemoveDirectory    = simpleRemove fs
  , fuseRename             = simpleRename fs
  , fuseCreateSymbolicLink = simpleCreateSymbolicLink fs
  , fuseReadSymbolicLink   = simpleReadSymbolicLink fs
  }

defStat = FileStat
  { statEntryType = Directory
  , statFileMode = 0
  , statLinkCount = 2
  , statFileOwner = 0
  , statFileGroup = 0
  , statSpecialDeviceID = 0
  , statFileSize = 4096
  , statBlocks = 1
  , statAccessTime = 0
  , statModificationTime = 0
  , statStatusChangeTime = 0
  }

dirStat = defStat
  { statFileMode = foldr1 unionFileModes
                     [ ownerReadMode
                     , ownerWriteMode
                     , ownerExecuteMode
                     , groupReadMode
                     , groupExecuteMode
                     , otherReadMode
                     , otherExecuteMode
                     ]
  , statLinkCount = 2
  }

fileStat len = defStat
  { statEntryType = RegularFile
  , statFileMode = foldr1 unionFileModes
                     [ ownerReadMode
                     , ownerWriteMode
                     , groupReadMode
                     , otherReadMode
                     ]
  , statLinkCount = 1
  , statFileSize = fromIntegral len
  }

linkStat = defStat
  { statEntryType = SymbolicLink
  , statFileMode = foldr1 unionFileModes
                     [ ownerReadMode
                     , ownerWriteMode
                     , ownerExecuteMode
                     , groupReadMode
                     , groupWriteMode
                     , groupExecuteMode
                     , otherReadMode
                     , otherWriteMode
                     , otherExecuteMode
                     ]
  , statLinkCount = 3
  , statFileSize = 4
  }

changeMode :: Int64 -> FileStat -> FileStat
changeMode mode st | mode > 0 = st { statFileMode = fromIntegral mode }
                   | otherwise = st

chown st ctx = st { statFileOwner = fuseCtxUserID ctx
                  , statFileGroup = fuseCtxGroupID ctx
                  }

simpleStatToFileStat :: SimpleStat -> Maybe FileStat
simpleStatToFileStat SimpleStat {..} =
  case simpleType of
    'D' -> Just $ changeMode simpleMode $ dirStat
    'F' -> Just $ changeMode simpleMode $ fileStat simpleSize
    'L' -> Just $ changeMode simpleMode $ linkStat
    _   -> Nothing

simpleGetFileStat :: FS -> FilePath -> IO (Either Errno FileStat)
simpleGetFileStat _ "/" = Right . chown dirStat <$> getFuseContext
simpleGetFileStat fs path = do
  ctx <- getFuseContext
  ret <- statFile fs path
  case ret of
    Left e -> return $ Left e
    Right stat -> case simpleStatToFileStat stat of
                    Nothing -> return $ Left eNOENT
                    Just st -> return . Right $ chown st ctx

simpleOpenDirectory _ _ = return eOK

simpleReadDirectory :: FS -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
simpleReadDirectory fs path = do
  ctx <- getFuseContext
  ret <- getDir fs path
  case ret of
    Left _ -> return $ Right (defaultDirs ctx)
    Right stats -> return $ Right (defaultDirs ctx ++ catMaybes (map (go ctx) stats))

  where defaultDirs ctx = [ (".",  chown dirStat ctx)
                          , ("..", chown dirStat ctx)
                          ]

        go ctx stat@(SimpleStat { simpleName = n }) =
          case simpleStatToFileStat stat of
            Nothing   -> Nothing
            Just stat -> Just (n, chown stat ctx)


simpleOpen :: FS -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno SimpleHandle)
simpleOpen _ _ WriteOnly _ = Right <$> newWriteHandle
simpleOpen fs path _ _ = do
  ret <- getFile fs path
  case ret of
    Left e    -> return $ Left e
    Right dat -> Right <$> newReadHandle dat

simpleRead :: FS -> FilePath -> SimpleHandle -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
simpleRead _ _ h byteCount offset = go <$> FS.simpleRead h (fromIntegral byteCount) (fromIntegral offset)
  where go (Left e)  = Left e
        go (Right s) = Right $ LB.toStrict s

simpleWrite :: FS -> FilePath -> SimpleHandle -> B.ByteString -> FileOffset -> IO (Either Errno ByteCount)
simpleWrite _ _ h dat offset = go <$> FS.simpleWrite h (LB.fromStrict dat) (fromIntegral offset)
  where go (Left e)  = Left e
        go (Right s) = Right $ fromIntegral s

simpleCreateDevice :: FS -> FilePath -> EntryType -> FileMode -> DeviceID -> IO Errno
simpleCreateDevice fs path Directory m _   = do
  ret <- putDir fs path
  case ret of
    eOK -> putMode fs path $ fromIntegral m
    _   -> return ret

simpleCreateDevice fs path RegularFile m _ = do
  ret <- putFile fs path LB.empty
  case ret of
    eOK -> putMode fs path $ fromIntegral m
    _   -> return ret

simpleCreateDevice fs path SymbolicLink m _ = do
  ret <- putFile fs path LB.empty
  ret2 <- putFile fs (fileTypePath path) "L"
  if ret == eOK && ret2 == eOK then putMode fs path $ fromIntegral m
                               else return ret

simpleCreateDevice _ _ _ _ _               = pure eNOSYS

simpleSetFileMode :: FS -> FilePath -> FileMode -> IO Errno
simpleSetFileMode fs path m = putMode fs path $ fromIntegral m

fileModePath :: FilePath -> FilePath
fileModePath path = "/.fs" ++ path <.> "mode"

fileTypePath :: FilePath -> FilePath
fileTypePath path = "/.fs" ++ path <.> "type"

putMode :: FS -> FilePath -> Int -> IO Errno
putMode fs path m = putFile fs (fileModePath path) (LB.fromStrict . B.pack $ show m)

simpleCreateDirectory :: FS -> FilePath -> FileMode -> IO Errno
simpleCreateDirectory fs path m = do
  ret <- putDir fs path
  case ret of
    eOK -> putMode fs path $ fromIntegral m
    _   -> return ret

simpleRemove :: FS -> FilePath -> IO Errno
simpleRemove fs path = do
  ret <- deleteFile fs path
  void $ deleteFile fs (fileModePath path)
  void $ deleteFile fs (fileTypePath path)
  return eOK

simpleRename :: FS -> FilePath -> FilePath -> IO Errno
simpleRename fs src dst = renameFile fs src dst

simpleCreateSymbolicLink :: FS -> FilePath -> FilePath -> IO Errno
simpleCreateSymbolicLink fs src dst = do
  ret <- putFile fs dst (LB.pack src)
  ret2 <- putFile fs (fileTypePath dst) "L"
  if ret == eOK && ret2 == eOK then return eOK
                               else return eACCES

simpleReadSymbolicLink :: FS -> FilePath -> IO (Either Errno FilePath)
simpleReadSymbolicLink fs dst = do
  ret <- getFile fs dst
  case ret of
    Left _    -> return $ Left eNOENT
    Right src -> return . Right $ LB.unpack src

simpleGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
simpleGetFileSystemStats _ =
  return $ Right $ FileSystemStats
    { fsStatBlockSize = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount = 5
    , fsStatFilesFree = 10
    , fsStatMaxNameLength = 255
    }