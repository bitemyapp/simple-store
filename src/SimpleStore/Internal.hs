module SimpleStore.Internal (
    putWriteStore
  , obtainLock
  , releaseLock
  , withLock
  , processExists
) where

import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import           Control.Monad.STM
import           SimpleStore.Types
import Control.Exception
import System.Posix.Process
import System.Posix.Types


putWriteStore :: SimpleStore st -> st -> IO ()
putWriteStore store state = atomically . (writeTVar tState) $ state
  where tState = storeState store

-- | Lock a simplestore from being able to be written to
obtainLock :: SimpleStore st -> IO StoreLock
obtainLock store = atomically . readTMVar . storeLock $ store

-- | Allow a simplestore to write to a lock
releaseLock :: SimpleStore st -> IO ()
releaseLock store = atomically $ putTMVar (storeLock store) StoreLock

withLock :: SimpleStore st -> IO b -> IO b
withLock store func = do
  obtainLock store
  res <- func
  releaseLock store
  return res

processExists :: Int -> IO Bool
processExists s = catch (getProcessPriority (CPid . fromIntegral $ s) >> return True) handleNotFound
  where
    handleNotFound :: SomeException -> IO Bool
    handleNotFound _ = return False
