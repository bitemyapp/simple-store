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
import Data.Either

-- | Get the current value of the store
getSimpleStore :: SimpleStore st -> IO st
getSimpleStore store = atomically . readTVar . storeState $ store

-- | Put a new value into a simple store with the lock
putSimpleStore :: SimpleStore st -> st -> IO ()
putSimpleStore store state = withLock store $ putWriteStore store state

-- | Open a simple store from a filepath reading in the newest most valid store
openSimpleStore :: S.Serialize st => FilePath -> IO (Either StoreError (SimpleStore st))
openSimpleStore fp = do
  dir <- makeAbsoluteFp fp
  exists <- isDirectory dir
  if exists
     then do lock <- attemptTakeLock fp
             if isRight lock
                then do dirContents <- listDirectory dir
                        print dirContents
                        let files = filter isState dirContents
                        print files
                        modifiedDates <-
                           traverse (\file -> do               -- Lambda is because the instance for Traversable on ()
                                        t <- getModified file  -- Traverses the second item so sequence only evaluates
                                        return (t,file)        -- the second item
                                       ) files
                        print modifiedDates
                        let sortedDates = snd <$> sortBy (compare `on` snd) modifiedDates
                        print sortedDates
                        openNewestStore createStoreFromFilePath sortedDates
                else return . Left $ StoreLocked
     else return . Left $ StoreFolderNotFound

-- | Initialize a simple store from a given filepath and state.
-- The filepath should just be to the directory you want the state created in
-- as in "state"
makeSimpleStore :: (S.Serialize st) => FilePath -> st -> IO (Either StoreError (SimpleStore st))
makeSimpleStore dir state = do
  fp <- initializeDirectory dir
  _ <- attemptTakeLock fp
  let encodedState = S.encode state
      checkpointPath = fp </> (fromText . pack $ (show initialVersion) ++ "checkpoint.st")
      initialVersion = 0
  writeFile checkpointPath encodedState
  handle <- openFile checkpointPath ReadWriteMode
  Right <$> createStore fp handle initialVersion state



attemptOpenDefault :: (S.Serialize st) => FilePath -> st -> IO (Either StoreError (SimpleStore st))
attemptOpenDefault fp initialState = do
  eStore <- openSimpleStore fp
  either (\_ -> makeSimpleStore fp initialState) (return . Right) eStore

-- | Release the file lock and close the handle to the file allowing another processes to open
-- the store
closeSimpleStore :: SimpleStore st -> IO ()
closeSimpleStore store = withLock store $ do
  closeStoreHandle store
  releaseFileLock store

-- | Run a function against the state and put the result into the state
-- This does not write the store to disk
modifySimpleStore :: SimpleStore st -> (st -> IO st) -> IO (Either StoreError ())
modifySimpleStore store func = withLock store $ do
  state <- readTVarIO tState
  res <- func state
  Right <$> (atomically $ writeTVar tState res)
  where tState = storeState store


-- | Write the current store to disk in the given folder
createCheckpoint :: (S.Serialize st) => SimpleStore st -> IO (Either StoreError ())
createCheckpoint store = withLock store $ checkpoint store

