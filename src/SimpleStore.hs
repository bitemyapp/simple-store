module SimpleStore (module SimpleStore) where

import           SimpleStore.IO    as SimpleStore (closeSimpleStore,
                                                   createCheckpoint,
                                                   getSimpleStore,
                                                   makeSimpleStore,
                                                   modifySimpleStore,
                                                   openSimpleStore,
                                                   putSimpleStore)
import           SimpleStore.Types as SimpleStore (SimpleStore, StoreError (..))
