{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module SimpleStore.FileIO where

import           Control.Applicative
import           Control.Exception
import           Control.Monad             hiding (sequence)
import qualified Data.ByteString           as BS
import           Data.Serialize
import           Data.Traversable
import           Filesystem                hiding (readFile, writeFile)
import           Filesystem.Path
import           Filesystem.Path.CurrentOS hiding (encode)
import           Prelude                   hiding (FilePath, sequence)
import           Safe
import           SimpleStore.Internal
import           SimpleStore.Types
import           System.IO.Error
import           System.Posix.Process
import Control.Concurrent.STM

ableToBreakLock :: FilePath -> IO (Either StoreError FilePath)
ableToBreakLock fp = do
  fileExists <- isFile fp
  if fileExists
    then do
      ePid <- readFile (encodeString fp) >>= return . readMay :: IO (Maybe Int)
      putStrLn . show $ ePid
      case ePid of
        Just pid -> do
          exists <- processExists pid
          if exists
            then return . Left . StoreIOError $ "Process holding open.lock is already running"
            else return $ Right fp
        Nothing -> return . Left . StoreIOError $ "Unable to parse open.lock"
    else return $ Right fp

ableToBreakLockError :: IOError -> Bool
ableToBreakLockError e
  | isAlreadyInUseError e = False
  | isDoesNotExistError e = True
  | isPermissionError e = False
  | otherwise = False

-- | Create a lock file with the current process pid in it
-- The lock file should already be empty or non existent
createLock :: FilePath -> IO (Either StoreError ())
createLock fp = do
  pid <- getProcessID
  catch (Right <$> writeFile (encodeString fp) (show pid)) showError
  where showError :: IOException -> IO (Either StoreError ())
        showError e = return . Left . StoreIOError . show $ e

attemptTakeLock :: FilePath -> IO (Either StoreError ())
attemptTakeLock baseFP = do
  let fp = baseFP </> (fromText "open.lock")
  allowBreak <- ableToBreakLock fp
  res <- sequence $ createLock <$> allowBreak
  return . join $ res

releaseFileLock :: SimpleStore st -> IO ()
releaseFileLock store = do
  fp <- (\fp -> directory fp </> (fromText "open.lock")) <$> (atomically . readTVar . storeFP $ store)
  exists <- isFile fp
  if exists then removeFile fp else return ()

catchStoreError :: IOError -> StoreError
catchStoreError e
  | isAlreadyInUseError e = StoreAlreadyOpen
  | isDoesNotExistError e = StoreFileNotFound
  | isPermissionError e = StoreFileNotFound
  | otherwise = StoreIOError . show $ e