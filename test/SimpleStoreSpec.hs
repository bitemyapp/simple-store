{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module SimpleStoreSpec (main, spec) where

import           Control.Applicative
import           Data.Traversable
import           Filesystem
import           Filesystem.Path
import           Prelude             hiding (sequence)
import           SimpleStore
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Making, creating checkpoints, closing, reopening" $ do
    it "should open an initial state, create checkpoints, and then open the state back up" $ do
      let x = 10 :: Int
          dir = "test-states"
      eStore <- makeSimpleStore dir x
      sequence $ getSimpleStore <$> eStore
      sequence $ createCheckpoint <$> eStore
      sequence $ createCheckpoint <$> eStore
      sequence $ createCheckpoint <$> eStore
      sequence $ createCheckpoint <$> eStore
      sequence $ createCheckpoint <$> eStore
      sequence $ createCheckpoint <$> eStore
      sequence $ closeSimpleStore <$> eStore
      eStore' <- openSimpleStore dir
      case eStore' of
        (Left err) -> fail "Unable to open local state"
        (Right store) -> do
          x' <- getSimpleStore store
          x `shouldBe` x'
  describe "Make, close, open, modify, close open, check value" $ do
    it "Should create a new state, close it, then open the state and modify the state, close the state, and finally open it and check the value" $ do
      let initial = 100 :: Int
          dir = "test-states"
      eStore <- makeSimpleStore dir initial
      sequence $ createCheckpoint <$> eStore
      sequence $ closeSimpleStore <$> eStore
      eStore' <- openSimpleStore dir :: IO (Either StoreError (SimpleStore Int))
      sequence $ createCheckpoint <$> eStore'
      sequence $ (\store -> modifySimpleStore store (\x -> return $ x+1)) <$> eStore'
      sequence $ createCheckpoint <$> eStore'
      sequence $ closeSimpleStore <$> eStore'
      eStore'' <- openSimpleStore dir
      case eStore'' of
        (Left err) -> fail "Unable to open local state"
        (Right store) -> do
          x' <- getSimpleStore store
          x' `shouldBe` initial
