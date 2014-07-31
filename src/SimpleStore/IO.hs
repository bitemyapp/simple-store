{-# LANGUAGE NoImplicitPrelude #-}
module SimpleStore.IO where

import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import           Control.Monad.STM
import           Data.Maybe                   (Maybe)
import           Filesystem
import           Filesystem.Path
import           Prelude                      hiding (FilePath, writeFile)
import           SimpleStore.Types
import           System.IO                    (Handle)
import Filesystem.Path.CurrentOS
import Data.Text
import SimpleStore.FileIO
import qualified Data.Serialize as S

getSimpleStore :: SimpleStore st -> IO st
getSimpleStore store = atomically . readTVar . storeState $ store

putSimpleStore :: SimpleStore st -> st -> IO ()
putSimpleStore store state = atomically . (writeTVar tState) $ state
  where tState = storeState store

openSimpleStore :: FilePath -> IO (Either StoreError (SimpleStore st))
openSimpleStore dir = undefined

makeSimpleStore :: (S.Serialize st) => FilePath -> st -> IO (Either StoreError (SimpleStore st))
makeSimpleStore dir state = do
  fp <- initializeDirectory dir
  lockRes <- attemptTakeLock fp
  st <- newTVarIO state
  lock <- newTMVarIO StoreLock
  let encodedState = S.encode state
  let checkpointPath = fp </> (fromText . pack $ "checkpoint" ++ (show initialVersion) ++ ".st")
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


createCheckpointSimpleStore
