{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module SimpleStore.FileIO where

import           Control.Applicative
import           Control.Concurrent.ReadWriteVar (RWVar)
import qualified Control.Concurrent.ReadWriteVar as RWVar
import           Control.Exception
import           Control.Monad             hiding (sequence)
import           Data.Bifunctor
import qualified Data.ByteString           as BS
import           Data.Serialize
import           Data.Text
import           Data.Traversable
import           Filesystem                hiding (readFile, writeFile)
import           Filesystem.Path
import           Filesystem.Path.CurrentOS hiding (decode, encode)
import           Prelude                   hiding (FilePath, sequence)
import           Safe
import           SimpleStore.Internal
import           SimpleStore.Types
import           System.IO                 (hClose,hFlush)
import           System.IO.Error
import           System.Posix.Process

-- | Return the given filepath if it is able to break the open.lock file
ableToBreakLock :: FilePath -> IO (Either StoreError FilePath)
ableToBreakLock fp = do
  fileExists <- isFile fp
  if fileExists
     then do
       ePid <- readMay <$> readFile (encodeString fp) :: IO (Maybe Int)
       case ePid of
         Just pid -> do
           exists <- processExists pid
           return $
             if exists
                then Left . StoreIOError $ "Process holding " ++ show fp ++ " is already running"
                else Right fp
         Nothing -> return . Left . StoreIOError $ "Unable to parse open.lock"
     else return $ Right fp

-- | Catch all errors that allow the lock to still be taken
ableToBreakLockError :: IOError -> Bool
ableToBreakLockError = isDoesNotExistError

{- Old version of 'ableToBreakLockError'

ableToBreakLockError :: IOError -> Bool
ableToBreakLockError e
  | isAlreadyInUseError e = False
  | isDoesNotExistError e = True
  | isPermissionError e = False
  | otherwise = False

-}

-- | Create a lock file with the current process pid in it
-- The lock file should already be empty or non existent
createLock :: FilePath -> IO (Either StoreError ())
createLock fp = do
  pid <- getProcessID
  catch (Right <$> writeFile (encodeString fp) (show pid)) showError
  where showError :: IOException -> IO (Either StoreError ())
        showError e = return . Left . StoreIOError . show $ e

-- | Attempt to create a lock inside of the given filepath
attemptTakeLock :: FilePath -> IO (Either StoreError ())
attemptTakeLock baseFP = do
  let fp = baseFP </> (fromText "open.lock")
  ableToBreakLock fp >>=
    either 
      (return . Left)
      createLock
--
-- | release the lock for a given store
releaseFileLock :: SimpleStoreRecord st -> IO () 
releaseFileLock store = do
  let fp = storeDir store </> fromText "open.lock"
  exists <- isFile fp
  putStrLn $ "releasing lock " ++ (show $ storeDir store </> fromText "open.lock")
  when exists $ removeFile fp >> putStrLn ("deleted " ++ (show $ storeDir store </> fromText "open.lock"))

-- Catch errors for storing so they aren't thrown
catchStoreError :: IOError -> StoreError
catchStoreError e
  | isAlreadyInUseError e = StoreAlreadyOpen
  | isDoesNotExistError e = StoreFileNotFound
  | isPermissionError e = StoreFileNotFound
  | otherwise = StoreIOError . show $ e

-- | Opens the newest store that doesn't throw an exception or give a StoreError back as a result
openNewestStore :: (a -> IO (Either StoreError b)) -> [a] -> IO (Either StoreError b)
openNewestStore _ [] = return . Left $ StoreFileNotFound
openNewestStore f (x:xs) = do
  res <- catch (f x) (hIOException f xs)
  case res of
    Left err -> print err >> openNewestStore f xs
    _ -> return res
  where  hIOException :: (a -> IO (Either StoreError b)) -> [a] -> IOException -> IO (Either StoreError b)
         hIOException func ys err = print err >> openNewestStore func ys

-- Attempt to open a store from a filepath
createStoreFromFilePath :: (Serialize st) => FilePath -> IO (Either StoreError (SimpleStore st))
createStoreFromFilePath fp = do
  let eVersion = getVersionNumber . filename $ fp
  fHandle <- openFile fp ReadWriteMode
  fConts <- BS.hGetContents fHandle
  sequence $ first (StoreIOError . show) $ (createStore $ directory fp) <$> 
                                           eVersion <*> 
                                           decode fConts

-- | Create a checkpoint for a store. This attempts to write the state to disk
-- If successful it updates the version, releases the old file handle, and deletes the old file
checkpoint :: (Serialize st) => SimpleStoreRecord st -> IO (Either StoreError (SimpleStoreRecord st))
checkpoint store = do
  let fp    = storeDir store
      state = storeState store
      oldVersion = storeCheckpointVersion store
      newVersion = (oldVersion + 1) `mod` 5
      encodedState = encode state
      oldCheckpointPath = fp </> fromText  ( Data.Text.append (pack . show $ oldVersion)  "checkpoint.st")
      checkpointPath = fp </> fromText  ( Data.Text.append ( pack.show $ newVersion)  "checkpoint.st")
  handle <- openFile checkpointPath ReadWriteMode
  eFileRes <- catch (Right <$> (BS.hPut handle encodedState >> hClose handle)) (return . Left . catchStoreError)  
  either
    (\err -> return $ Left err)
    (\result -> do
      removeFile oldCheckpointPath
      return $ Right $ store { storeCheckpointVersion = newVersion })
    eFileRes

-- Initialize a directory by adding the working directory and checking if it already exists.
-- If the folder already exists it deletes it and creates a new directory
initializeDirectory :: FilePath -> IO (Either StoreError FilePath)
initializeDirectory dir = do
  fp <- makeAbsoluteFp dir
  exists <- (||) <$> isFile fp <*> isDirectory fp
  if exists
    then return $ Left StoreDirectoryAlreadyExists
    else 
      do
        createDirectory True fp
        fmap (const fp) <$> attemptTakeLock fp

makeAbsoluteFp :: FilePath -> IO FilePath
makeAbsoluteFp fp = do
  if absolute fp
    then return fp
    else do
      base <- getWorkingDirectory
      return $ base </> fp

