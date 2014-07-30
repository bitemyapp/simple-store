{-# LANGUAGE NoImplicitPrelude #-}

module SimpleStore.FileIO where

import           Control.Applicative
import qualified Data.ByteString     as BS
import           Data.Serialize
import           Data.Traversable
import           Filesystem
import           Filesystem.Path
import           Prelude             hiding (FilePath, readFile, writeFile, sequence)
import           SimpleStore.Types
import           System.IO.Error
import SimpleStore.Internal
import Control.Exception

ableToBreakLock :: FilePath -> IO (Either String ())
ableToBreakLock fp = do
  fileExists <- isFile fp
  if fileExists
    then do
      ePid <- readFile fp >>= return . decode
      exists <- sequence $ processExists <$> ePid
      return $ removeFalse exists
    else return $ Right ()
  where removeFalse :: Either String Bool -> Either String ()
        removeFalse (Right False) = Left "Lock already exists and process is still running"
        removeFalse (Right True) = Right ()
        removeFalse (Left s) = Left s


ableToBreakLockError :: IOError -> Bool
ableToBreakLockError e
  | isAlreadyInUseError e = False
  | isDoesNotExistError e = True
  | isPermissionError e = False
  | otherwise = False

-- | Create a lock file with the current process pid in it
-- The lock file should already be empty or non existent
createLock :: FilePath -> Int -> IO (Either String ())
createLock fp pid = do
  catch (Right <$> writeFile fp (encode pid)) showError
  where showError :: IOException -> IO (Either String ())
        showError e = return . Left . show $ e