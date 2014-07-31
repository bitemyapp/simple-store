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
  describe "someFunction" $ do
    it "should work fine" $ do
      let x = 10 :: Int
          dir = "test-states"
      eStore <- makeSimpleStore dir x
      sequence $ getSimpleStore <$> eStore
      putStrLn "Hello1"
      sequence $ createCheckpoint <$> eStore
      putStrLn "Hello2"
      sequence $ createCheckpoint <$> eStore
      putStrLn "Hello3"
      sequence $ createCheckpoint <$> eStore
      putStrLn "Hello4"
      sequence $ createCheckpoint <$> eStore
      putStrLn "Hello5"
      sequence $ createCheckpoint <$> eStore
      putStrLn "Hello6"
      sequence $ createCheckpoint <$> eStore
      putStrLn "Hello7"
      sequence $ closeSimpleStore <$> eStore
      putStrLn "Closed"
      eStore' <- openSimpleStore dir
      putStrLn "Hello9"
      case eStore' of
        (Left err) -> fail "Unable to open local state"
        (Right store) -> do
          putStrLn "Hello10"
          x' <- getSimpleStore store
          putStrLn "Hello11"
          x `shouldBe` x'
