module SimpleStore (module SimpleStore) where

import           SimpleStore.IO    as SimpleStore (closeSimpleStore,
                                                   createCheckpoint,
                                                   getSimpleStore,
                                                   makeSimpleStore,
                                                   modifySimpleStore,
                                                   openSimpleStore,
                                                   attemptOpenDefault,
                                                   putSimpleStore)
import           SimpleStore.Types as SimpleStore (SimpleStore, StoreError (..))
