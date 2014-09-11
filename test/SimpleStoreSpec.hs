{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module SimpleStoreSpec (main, spec) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMVar
import           Data.Either
import           Data.Traversable
import           Data.Traversable
import           Filesystem
import           Filesystem.Path
import           Prelude                      hiding (sequence)
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
      workingDir <- getWorkingDirectory
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
          removeTree $ workingDir </> dir
          x `shouldBe` x'
  describe "Make, close, open, modify, close open, check value" $ do
    it "Should create a new state, close it, then open the state and modify the state, close the state, and finally open it and check the value" $ do
      let initial = 100 :: Int
          modifyX x = x + 1000
          dir = "test-states"
      workingDir <- getWorkingDirectory
      (Right store) <- makeSimpleStore dir initial
      createCheckpoint store
      closeSimpleStore store
      (Right store') <- openSimpleStore dir :: IO (Either StoreError (SimpleStore Int))
      createCheckpoint store'
      (\store -> modifySimpleStore store (return . modifyX)) store'
      createCheckpoint store'
      closeSimpleStore store'
      eStore'' <- openSimpleStore dir :: IO (Either StoreError (SimpleStore Int))
      case eStore'' of
        (Left err) -> fail "Unable to open local state"
        (Right store) -> do
          x' <- getSimpleStore store'
          removeTree $ workingDir </> dir
          x' `shouldBe` (modifyX initial)
  describe "Async updating/creating checkpoints for a state" $ do
    it "Should start 100 threads trying to update a state and should modify the state correctly, be able to close and reopen the state, and then read the correct value" $ do
      let initial = 0 :: Int
          modifyX = (+2)
          dir = "test-states"
          functions = replicate 100  (\tv x -> (atomically $ readTMVar tv) >> (return . modifyX $ x))
      waitTVar <- newEmptyTMVarIO
      (Right store) <- makeSimpleStore dir initial
      
      createCheckpoint store
      aRes <- traverse (\func -> async $ do
                          modifySimpleStore store (func waitTVar)
                          createCheckpoint store) functions
      atomically $ putTMVar waitTVar ()
      results <- traverse wait aRes
      x'' <- getSimpleStore store
      putStrLn $ "x -> " ++ (show x'')
      createCheckpoint store
      closeSimpleStore store
      eStore <- openSimpleStore dir
      let store' = either (error . show) id eStore
      x' <- getSimpleStore store'
      x' `shouldBe` (200 :: Int)

