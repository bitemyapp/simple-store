{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
module SimpleStore.Types where

import           Control.Applicative
import           Control.Concurrent.ReadWriteVar
import           Control.Monad.IO.Class 
import           System.IO (Handle)
import           Control.Monad.Trans.State
import Prelude hiding (FilePath)
import Filesystem.Path


newtype SimpleStore st = SimpleStore { simpleStoreVar :: RWVar (SimpleStoreRecord st) }

data SimpleStoreRecord st = SimpleStoreRecord {
    storeDir               :: !FilePath
  , storeState             :: !st
  , storeCheckpointVersion :: !Int
}

data StoreLock = ReadOnly | ReadWrite

data StoreError = StoreAlreadyOpen | StoreClosed | StoreLocked | StoreFolderNotFound | StoreFileNotFound | StoreCheckpointNotFound | StoreDirectoryAlreadyExists | StoreIOError String deriving (Show, Eq)

newtype StoreM (stack :: [(StoreLock, *)]) a = StoreM { whileLocked :: StateT (StoreStack stack) IO a }

instance Functor (StoreM stack) where
  fmap f = StoreM . fmap f . whileLocked
 
instance Applicative (StoreM stack) where
  pure = StoreM . pure
  storeF <*> storeX = StoreM $ whileLocked storeF <*> whileLocked storeX 

instance Monad (StoreM stack) where
  return = pure
  storeM >>= storeMF = StoreM $ (whileLocked storeM >>= whileLocked . storeMF)

instance MonadIO (StoreM stack) where
  liftIO = StoreM . liftIO

data StoreStack (stack :: [(StoreLock, *)]) where
  StoreNil :: StoreStack '[]
  StoreCons :: SimpleStoreRecord st -> StoreStack stack -> StoreStack ('(writeFlag, st) ': stack)

data StoreIndex (stack :: [(StoreLock, *)]) (writeFlag :: StoreLock) (st :: *) where
  StoreHere :: StoreIndex ('(writeFlag, st) ': rest) writeFlag st
  StoreThere :: StoreIndex rest writeFlag storeSt -> StoreIndex ('(nextWriteFlag, nextSt) ': rest) writeFlag storeSt

