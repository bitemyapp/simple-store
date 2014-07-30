module SimpleStore.Types where

import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import           System.IO (Handle)


data SimpleStore st = SimpleStore {
    storeFP       :: FilePath        -- Full filepath to the directory
  , storeFileName :: TVar FilePath   -- Current filepath to the open file
  , storeState    :: TVar st         -- Current open state of the file
  , storeLock     :: TMVar StoreLock -- Lock to handle modifying operations to the file
  , storeHandle   :: TMVar Handle    -- Filehandle to the current file version
}

data StoreLock = StoreLock

data StoreError = StoreAlreadyOpen | StoreLocked deriving (Show, Eq)