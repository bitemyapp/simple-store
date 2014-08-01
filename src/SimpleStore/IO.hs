{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module SimpleStore.IO where

import           Control.Applicative
import           Control.Concurrent.STM.TVar
import           Control.Monad                hiding (sequence)
import           Control.Monad.STM
import           Data.Function
import           Data.List
import qualified Data.Serialize               as S
import           Data.Text                    hiding (filter, foldl, map,
                                               maximum, stripPrefix)
import           Data.Traversable
import           Filesystem
import           Filesystem.Path
import           Filesystem.Path.CurrentOS    hiding (decode)
import           Prelude                      hiding (FilePath, sequence,
                                               writeFile)
import           SimpleStore.FileIO
import           SimpleStore.Internal
import           SimpleStore.Types

-- | Get the va
getSimpleStore :: SimpleStore st -> IO st
getSimpleStore store = atomically . readTVar . storeState $ store

putSimpleStore :: SimpleStore st -> st -> IO ()
putSimpleStore store state = withLock store $ putWriteStore store state

openSimpleStore :: (S.Serialize st) => FilePath -> IO (Either StoreError (SimpleStore st))
openSimpleStore fp = do
  dir <- (</> fp) <$> getWorkingDirectory
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
      let sortedDates = snd <$> sortBy (compare `on` snd) modifiedDates
      print sortedDates
      openNewestStore createStoreFromFilePath sortedDates
    else return . Left $ StoreFolderNotFound

makeSimpleStore :: (S.Serialize st) => FilePath -> st -> IO (Either StoreError (SimpleStore st))
makeSimpleStore dir state = do
  fp <- initializeDirectory dir
  let encodedState = S.encode state
      checkpointPath = fp </> (fromText . pack $ (show initialVersion) ++ "checkpoint.st")
      initialVersion = 0
  writeFile checkpointPath encodedState
  handle <- openFile checkpointPath ReadWriteMode
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

createCheckpoint :: (S.Serialize st) => SimpleStore st -> IO (Either StoreError ())
createCheckpoint store = withLock store $ checkpoint store
