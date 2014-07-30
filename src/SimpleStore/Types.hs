module SimpleStore.Types where

import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import           Data.Maybe (Maybe)
import           GHC.IO.Handle


data SimpleStore st = SimpleStore {
    storeFP     :: FilePath
  , storeState  :: TVar st
  , storeLock   :: TMVar StoreLock
  , storeHandle :: Maybe Handle
}

data StoreLock = StoreLock
