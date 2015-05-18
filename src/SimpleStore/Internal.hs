{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module SimpleStore.Internal (
    processExists
  , getVersionNumber
  , createStore
  , isState
) where

import           Control.Applicative
import           Control.Concurrent.ReadWriteVar (RWVar)
import qualified Control.Concurrent.ReadWriteVar as RWVar
import           Control.Exception
import           Control.Monad hiding (sequence)
import           Data.Bifunctor
import           Data.Text
import           Data.Text.Read
import           Filesystem.Path
import           Filesystem.Path.CurrentOS
import           Prelude                      hiding (FilePath, sequence)
import           SimpleStore.Types
import           System.Posix.Process
import           System.Posix.Types
import           System.IO                    (Handle, hClose)


-- | Check if a process exists
processExists :: Int -> IO Bool
processExists s = catch (getProcessPriority (CPid . fromIntegral $ s) >> return True) handleNotFound
  where
    handleNotFound :: IOException -> IO Bool
    handleNotFound _ = return False

-- Get the version number of a file from the filepath
getVersionNumber :: FilePath -> Either String Int
getVersionNumber fp = second fst $ join $ decimal <$> eTextFp
  where eTextFp = first unpack $ toText fp

-- Create a store from it's members. Just creates the necessary TMVars/TVars
createStore :: FilePath -> Int -> st -> IO (SimpleStore st)
createStore fp version st = 
  SimpleStore <$> (RWVar.new $ SimpleStoreRecord fp st version)

-- Checks the extension of a filepath for ".st"
isState :: FilePath -> Bool
isState fp = extension fp == Just "st"

