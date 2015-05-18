{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module SimpleStoreSpec (main
                       , spec
                       , makeTestStore) where

import           Control.Applicative
import           Control.Concurrent (myThreadId)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.IO.Class (liftIO)
-- import           Control.Concurrent.STM.TMVar
import           Control.Monad (when)
import           Data.Either
import           Data.Traversable
-- import           Data.Traversable
import           Filesystem
import           Filesystem.Path
import           Prelude                      hiding (sequence)
import           SimpleStore
import           Test.Hspec

main :: IO ()
main = hspec spec

makeTestStore = do 
   let x = 10 :: Int
       dir = "test-states"
   workingDir <- getWorkingDirectory
   eStore <- makeSimpleStore dir x 
   return (eStore,dir,x,workingDir)


makeTestTextStore = do 
   let x = "10" :: String
       dir = "test-states"
   workingDir <- getWorkingDirectory
   eStore <- makeSimpleStore dir x 
   return (eStore,dir,x,workingDir)

spec :: Spec
spec = do
  describe "Making, creating checkpoints, closing, reopening" $ do
    it "should open an initial state, create checkpoints, and then open the state back up" $ do
      workingDir <- getWorkingDirectory
      removeTree $ workingDir </> "test-states"
      -- let x = 10 :: Int
      --     dir = "test-states"
      -- workingDir <- getWorkingDirectory
      -- eStore <- makeSimpleStore dir x
      (eStore,dir,x,workingDir)  <- makeTestStore
      either (const $ return ())
             (\store -> do
                 runStoreM $ withWriteLock store $ do
                   checkpointSimpleStore StoreHere
                 closeSimpleStore store)
             eStore
      eStore' <- openSimpleStore $ workingDir </> dir
      case eStore' of
        (Left err) -> fail $ "Unable to open local state: " ++ show err
        (Right store) -> do
          (Right x') <- runStoreM $ withReadLock store $ readSimpleStore StoreHere
          closeSimpleStore store
          removeTree $ workingDir </> dir
          x `shouldBe` x'
  describe "Make, close, open, modify, close open, check value" $ do
    it "Should create a new state, close it, then open the state and modify the state, close the state, and finally open it and check the value" $ do
      let initial = 100 :: Int
          modifyX x = x + 1000
          dir = "test-states"
      workingDir <- getWorkingDirectory
      (Right store) <- makeSimpleStore dir initial
      runStoreM $ withWriteLock store $ checkpointSimpleStore StoreHere
      closeSimpleStore store
      (Right store') <- openSimpleStore dir :: IO (Either StoreError (SimpleStore Int))
      runStoreM $ withWriteLock store $ do
        checkpointSimpleStore StoreHere
        x' <- readSimpleStore StoreHere
        writeSimpleStore StoreHere (modifyX x')
        checkpointSimpleStore StoreHere 
      closeSimpleStore store'
      eStore'' <- openSimpleStore dir :: IO (Either StoreError (SimpleStore Int))
      case eStore'' of
        (Left err) -> fail "Unable to open local state"
        (Right store) -> do
          (Right x') <- runStoreM $ withReadLock store $ readSimpleStore StoreHere
          closeSimpleStore store 
          removeTree $ workingDir </> dir
          x' `shouldBe` (modifyX initial)
  describe "Async updating/creating checkpoints for a state" $ do
    it "Should start 100 threads trying to update a state and should modify the state correctly, be able to close and reopen the state, and then read the correct value" $ do
      let initial = 0 :: Int
          modifyX = (+2)
          dir = "test-states"
          functions = replicate 100  (\x -> (return . modifyX $ x))
      waitTVar <- newEmptyTMVarIO
      (Right store) <- makeSimpleStore dir initial
      runStoreM $ withWriteLock store $ checkpointSimpleStore StoreHere
      aRes <- traverse (\func -> async $ do
                          atomically $ readTMVar waitTVar
                          runStoreM $ withWriteLock store $ do
                            x <- readSimpleStore StoreHere 
                            x' <- liftIO $ func x
                            writeSimpleStore StoreHere x'
                            checkpointSimpleStore StoreHere)
                       functions
      atomically $ putTMVar waitTVar ()
      results <- traverse wait aRes
      x'' <- runStoreM $ withReadLock store $ readSimpleStore StoreHere
      putStrLn $ "x -> " ++ (show x'')
      runStoreM $ withWriteLock store $ checkpointSimpleStore StoreHere
      closeSimpleStore store
      eStore <- openSimpleStore dir :: IO (Either StoreError (SimpleStore Int))
      let store' = either (error . show) id eStore
      (Right x') <- runStoreM $ withReadLock store $ readSimpleStore StoreHere
      x' `shouldBe` (200 :: Int)
      

