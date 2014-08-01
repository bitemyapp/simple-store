module SimpleStore (module SimpleStore) where

import           SimpleStore.IO as SimpleStore (getSimpleStore, putSimpleStore, openSimpleStore, makeSimpleStore, closeSimpleStore, modifySimpleStore, createCheckpoint)
import           SimpleStore.Types as SimpleStore (SimpleStore, StoreError(..)) 
