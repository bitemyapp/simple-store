module SimpleStore.IO where

import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import           Control.Monad.STM
import           Data.Maybe                   (Maybe)
import           SimpleStore.Types
import           System.IO                    (Handle)

getSimpleStore :: SimpleStore st -> IO st
getSimpleStore store = atomically . readTVar . storeState $ store

putSimpleStore :: SimpleStore st -> st -> IO ()
putSimpleStore store state = atomically . (writeTVar tState) $ state
  where tState = storeState store

openSimpleStore :: FilePath -> IO (Either StoreError (SimpleStore st))
openSimpleStore fp = undefined

makeSimpleStore :: FilePath -> st -> IO (Either StoreError (SimpleStore st))
makeSimpleStore fp state = undefined

closeSimpleStore :: SimpleStore st -> IO ()
closeSimpleStore store = undefined

modifySimpleStore :: SimpleStore st -> (st -> IO st) -> IO (Either StoreError ())
modifySimpleStore store func = undefined
