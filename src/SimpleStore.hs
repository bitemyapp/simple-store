module SimpleStore (module SimpleStore) where

import           SimpleStore.IO    as SimpleStore (runStoreM, withReadLock, withWriteLock, readSimpleStore, writeSimpleStore, checkpointSimpleStore, openSimpleStore, makeSimpleStore, attemptOpenDefaultSimpleStore, closeSimpleStore)
import           SimpleStore.Types as SimpleStore (SimpleStore, StoreError (..), StoreIndex(..))
