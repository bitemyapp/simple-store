{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module SimpleStore.IO where

import           Control.Applicative
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Monad                hiding (sequence)
import           Control.Monad.STM
import           Data.Bifunctor
import qualified Data.ByteString              as BS
import           Data.Function
import           Data.List
import           Data.Maybe                   (Maybe)
import           Data.Maybe
import           Data.Serialize
import           Data.Serialize
import qualified Data.Serialize               as S
import           Data.Text                    hiding (filter, foldl, map,
                                               maximum, stripPrefix)
import           Data.Text.Read
import           Data.Time.Clock
import           Data.Traversable
import           Filesystem
import           Filesystem.Path
import           Filesystem.Path.CurrentOS    hiding (decode)
import           Prelude                      hiding (FilePath, sequence,
                                               writeFile)
import           Safe
import           SimpleStore.FileIO
import           SimpleStore.Internal
import           SimpleStore.Types
import           System.IO                    (Handle, hClose)

getSimpleStore :: SimpleStore st -> IO st
getSimpleStore store = atomically . readTVar . storeState $ store

putSimpleStore :: SimpleStore st -> st -> IO ()
putSimpleStore store state = withLock store $ putWriteStore store state

openSimpleStore :: (S.Serialize st) => FilePath -> IO (Either StoreError (SimpleStore st))
openSimpleStore dir = do
  exists <- isDirectory dir
  if exists
    then do
      dirContents <- listDirectory dir
      print dirContents
      let files = filter isState dirContents
      print files
      modifiedDates <- traverse (\file -> do                -- Lambda is because the instance for Traversable on ()
                                    t <- getModified file   -- Traverses the second item so sequence only evaluates
                                    return (t,file)) files  -- the second item
      print modifiedDates
      let sortedDates = sortBy (compare `on` snd) modifiedDates

      case maximumMay modifiedDates of
        (Just (_, fp)) -> do

          --createStoreFromFilePath :: FilePath -> IO (Either StoreError (SimpleStore st))
          --createStoreFromFilePath fp = do
            let eVersion = getVersionNumber . filename $ fp
            fHandle <- openFile fp ReadWriteMode
            fConts <- BS.hGetContents fHandle
            let dec = decode fConts
            sequence $ first (StoreIOError . show) $  (createStore fp fHandle) <$> eVersion <*> dec
        Nothing -> return . Left $ StoreCheckpointNotFound
    else return . Left $ StoreFolderNotFound

makeSimpleStore :: (S.Serialize st) => FilePath -> st -> IO (Either StoreError (SimpleStore st))
makeSimpleStore dir state = do
  fp <- initializeDirectory dir
  let encodedState = S.encode state
      checkpointPath = fp </> (fromText . pack $ (show initialVersion) ++ "checkpoint.st")
      initialVersion = 0
  writeFile checkpointPath encodedState
  handle <- openFile checkpointPath AppendMode
  Right <$> createStore fp handle initialVersion state


initializeDirectory :: FilePath -> IO FilePath
initializeDirectory dir = do
  workingDir <- getWorkingDirectory
  let fp = workingDir </> dir
  exists <- isDirectory fp
  if exists
    then removeTree fp
    else return ()
  createDirectory True fp
  return fp



closeSimpleStore :: SimpleStore st -> IO ()
closeSimpleStore store = withLock store $ do
  closeStoreHandle store
  releaseFileLock store


modifySimpleStore :: SimpleStore st -> (st -> IO st) -> IO (Either StoreError ())
modifySimpleStore store func = withLock store $ do
  state <- readTVarIO tState
  res <- func state
  Right <$> (atomically $ writeTVar tState res)
  where tState = storeState store

createCheckpoint :: (Serialize st) => SimpleStore st -> IO (Either StoreError ())
createCheckpoint store = withLock store $ do
  fp <- directory <$> (readTVarIO . storeFP $ store)
  state <- readTVarIO tState
  newVersion <- (+1) <$> (readTVarIO tVersion)
  let encodedState = S.encode state
      checkpointPath = fp </> (fromText . pack $ (show newVersion) ++ "checkpoint.st")
  newHandle <- openFile checkpointPath ReadWriteMode
  eFileRes <- catch (Right <$> writeFile checkpointPath encodedState) (return . Left . catchStoreError)
  updateIfWritten eFileRes newVersion newHandle
  where tState = storeState store
        tVersion = storeCheckpointVersion store
        tHandle = storeHandle store
        updateIfWritten l@(Left _) _ _= return l
        updateIfWritten _ version fHandle = do
          atomically $ do
            writeTVar tVersion version
            takeTMVar tHandle >> putTMVar tHandle fHandle
          return . Right $ ()
