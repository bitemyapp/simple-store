{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module SimpleStore.Internal (
    putWriteStore
  , obtainLock
  , releaseLock
  , withLock
  , processExists
  , getVersionNumber
  , createStore
  , isState
  , closeStoreHandle
) where

import           Control.Applicative
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Monad hiding (sequence)
import           Control.Monad.STM
import           Data.Bifunctor
import           Data.Text
import           Data.Text.Read
import           Filesystem.Path
import           Filesystem.Path.CurrentOS
import           Prelude                      hiding (FilePath, sequence)
import           Data.Traversable
import           SimpleStore.Types
import           System.Posix.Process
import           System.Posix.Types
import           System.IO                    (Handle, hClose)



putWriteStore :: SimpleStore st -> st -> IO ()
putWriteStore store state = atomically . (writeTVar tState) $ state
  where tState = storeState store

-- | Lock a simplestore from being able to be written to
obtainLock :: SimpleStore st -> IO StoreLock
obtainLock store = atomically . takeTMVar . storeLock $ store

-- | Allow a simplestore to write to a lock
releaseLock :: SimpleStore st -> IO ()
releaseLock store = atomically $ putTMVar (storeLock store) StoreLock

withLock :: SimpleStore st -> IO b -> IO b
withLock store func = do
  obtainLock store
  res <- finally (func) (releaseLock store)
  return res

processExists :: Int -> IO Bool
processExists s = catch (getProcessPriority (CPid . fromIntegral $ s) >> return True) handleNotFound
  where
    handleNotFound :: IOException -> IO Bool
    handleNotFound _ = return False

getVersionNumber :: FilePath -> Either String Int
getVersionNumber fp = second fst $ join $ decimal <$> eTextFp
  where eTextFp = first unpack $ toText fp


createStore :: FilePath -> Handle -> Int -> st -> IO (SimpleStore st)
createStore fp fHandle version st = do
  sState <- newTVarIO st
  sLock <- newTMVarIO StoreLock
  sHandle <- newTMVarIO fHandle
  sVersion <- newTVarIO version
  sFp <- newTVarIO fp
  return $ SimpleStore sFp sState sLock sHandle sVersion

isState :: FilePath -> Bool
isState fp = case extension fp of
              (Just ext) -> if ext == "st" then True else False
              Nothing -> False

closeStoreHandle :: SimpleStore st -> IO ()
closeStoreHandle store = do
  fHandle <- atomically . readTMVar . storeHandle $ store
  hClose fHandle