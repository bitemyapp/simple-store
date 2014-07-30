module SimpleStore.IO where

import           Control.Concurrent.STM.TVar
import           Control.Monad.STM
import           SimpleStore.Internal
import           SimpleStore.Types

getSimpleStore :: SimpleStore st -> IO st
getSimpleStore store = atomically . readTVar . storeState $ store

putSimpleStore :: SimpleStore st -> st -> IO ()
putSimpleStore store state = withLock store $ putWriteStore store state

openSimpleStore :: FilePath -> IO (Either StoreError (SimpleStore st))
openSimpleStore fp = undefined

makeSimpleStore :: FilePath -> st -> IO (Either StoreError (SimpleStore st))
makeSimpleStore fp state = undefined

closeSimpleStore :: SimpleStore st -> IO ()
closeSimpleStore store = undefined

modifySimpleStore :: SimpleStore st -> (st -> IO st) -> IO (Either StoreError ())
modifySimpleStore store func = withLock store $ do
  state <- atomically . readTVar . storeState $ store
  state' <- func state
  putSimpleStore store state'
  return . Right $ ()