{-# LANGUAGE NoImplicitPrelude #-}
module SimpleStore.Types where

import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import           Data.Maybe (Maybe)
import           System.IO (Handle)
import Prelude hiding (FilePath)
import Filesystem.Path


data SimpleStore st = SimpleStore {
    storeFP                :: FilePath
  , storeState             :: TVar st
  , storeLock              :: TMVar StoreLock
  , storeHandle            :: TVar Handle
  , storeCheckpointVersion :: Int
}

data StoreLock = StoreLock

data StoreError = StoreAlreadyOpen | StoreLocked | StoreFolderNotFound | StoreCheckpointNotFound | StoreIOError String deriving (Show, Eq)