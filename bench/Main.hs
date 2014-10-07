{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DisambiguateRecordFields  #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}


module Main where

import           Control.Monad             hiding (sequence)
import           CorePrelude
import           Data.Char
import           Data.Either
import           Data.Serialize
import           Data.Traversable
import           Filesystem
import           Filesystem.Path.CurrentOS hiding (decode)
import           Prelude                   (map, mapM_, replicate, getLine)
import           SimpleStore
import           System.Random

main :: IO ()
main = do
  benchN 100 100
  putStrLn "Finished bench"
  putStrLn "Goodbye"





-- benchN :: Int -> IO [String]
benchN :: Int -> Int -> IO () --  [(SimpleStore [Int])]
benchN n times = do
  folders <- sequence $ replicate n $ randomString 10
  createDirectory True "benchmark-states"
  rList <- randomList 200
  simpleStores <- rights <$> traverse (\fp -> makeSimpleStore ("benchmark-states" </> decodeString fp) rList) folders
  replicateM times $ do
    modifyStores simpleStores modify
    createCheckpoints simpleStores
  putStrLn "Finished replicate"
  closeStores simpleStores
  return ()


addN :: Int -> Int -> IO Int
addN n x = return $ n + x

modify _ =  randomList 200

createCheckpoints :: (Serialize st) => [(SimpleStore st)] -> IO ()
createCheckpoints = mapM_ createCheckpoint


modifyStores :: [(SimpleStore st)] -> (st -> IO st) -> IO ()
modifyStores sts f =  mapM_ (\st -> modifySimpleStore st f) sts

closeStores :: [(SimpleStore st)] -> IO ()
closeStores sts = mapM_ closeSimpleStore sts



randomString :: Int -> IO String
randomString n = do
  ioRands <- sequence  $ replicate n $ randomRIO (97,120)
  return $ map toEnum ioRands


randomList :: Int -> IO [Int]
randomList n = replicateM n $ randomIO