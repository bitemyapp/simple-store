module SimpleStore (
    getSimpleStore
  , putSimpleStore
  , openSimpleStore
  , makeSimpleStore
  , closeSimpleStore
  , modifySimpleStore
  , SimpleStore
  , StoreError(..)
) where

import           SimpleStore.IO
import           SimpleStore.Types
