{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module SimpleStore.IO where

import           Control.Applicative
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import           Control.Monad                hiding (sequence)
import           Control.Monad.STM
import           Data.Bifunctor
import qualified Data.ByteString              as BS
import           Data.Maybe                   (Maybe)
import           Data.Maybe
import           Data.Serialize
import qualified Data.Serialize               as S
import           Data.Text                    hiding (filter, map, maximum,
                                               stripPrefix)
import           Data.Text.Read
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
import           System.IO                    (Handle)

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
      modifiedDates <- traverse (\file -> do
                                    t <- getModified file
                                    return (t,file)) files
      print modifiedDates
      case maximumMay modifiedDates of
        (Just (_, lastFile)) -> do
          let eVersion = getVersionNumber . filename $ lastFile
          fHandle <- openFile lastFile ReadWriteMode
          fConts <- BS.hGetContents fHandle
          let dec = decode fConts
          sequence $ first (StoreIOError . show) $  (createStore lastFile fHandle) <$> eVersion <*> dec
        Nothing -> return . Left $ StoreCheckpointNotFound
    else return . Left $ StoreFolderNotFound


makeSimpleStore :: (S.Serialize st) => FilePath -> st -> IO (Either StoreError (SimpleStore st))
makeSimpleStore dir state = do
  fp <- initializeDirectory dir
  lockRes <- attemptTakeLock fp
  st <- newTVarIO state
  lock <- newTMVarIO StoreLock
  let encodedState = S.encode state
  let checkpointPath = fp </> (fromText . pack $ (show initialVersion) ++ "checkpoint.st")
  writeFile checkpointPath encodedState
  handle <- openFile checkpointPath AppendMode
  tHandle <- newTVarIO handle
  return . Right $ SimpleStore fp st lock tHandle initialVersion
  where initialVersion = 0


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
closeSimpleStore store = undefined

modifySimpleStore :: SimpleStore st -> (st -> IO st) -> IO (Either StoreError ())
modifySimpleStore store func = undefined

createCheckpoint :: SimpleStore st -> IO (Either StoreError ())
createCheckpoint store = undefined
