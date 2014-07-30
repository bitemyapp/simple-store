module SimpleStore.Types where

import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import           Data.Maybe (Maybe)
import           System.IO (Handle)


data SimpleStore st = SimpleStore {
    storeFP     :: FilePath
  , storeState  :: TVar st
  , storeLock   :: TMVar StoreLock
  , storeHandle :: TMVar Handle
}

data StoreLock = StoreLock

data StoreError = StoreAlreadyOpen | StoreLocked deriving (Show, Eq)