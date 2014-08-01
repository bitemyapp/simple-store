module SimpleStore (
    getSimpleStore
  , putSimpleStore
  , openSimpleStore
  , makeSimpleStore
  , closeSimpleStore
  , modifySimpleStore
  , SimpleStore
  , StoreError(..)
  , createCheckpoint
) where

import           SimpleStore.IO
import           SimpleStore.Types
