{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import SimpleStoreSpec (makeTestStore)
-- import           Control.Concurrent


-- import           Control.Concurrent.STM.TMVar
import Control.Concurrent (threadDelay)
import           Data.Either
import           Data.Traversable
-- import           Data.Traversable
import           Filesystem
import           Filesystem.Path
import           Prelude                      hiding (sequence)
import           SimpleStore

import Control.Monad (guard)

main :: IO () 
main = do (eStore,dir,x,workingDir) <- makeTestStore
          putStrLn "starting memory loop"
          runMemTestLoop 0 eStore dir x workingDir

loopMax = 10000

runMemTestLoop count eStore dir x workingDir 
  | count > loopMax = (removeTree $ workingDir </> dir) >>
                      return () 
  | otherwise = do 
       threadDelay (1*1000*1000q)
       sequence $ getSimpleStore <$> eStore
       sequence $ createCheckpoint <$> eStore
       sequence $ createCheckpoint <$> eStore
       sequence $ createCheckpoint <$> eStore
       sequence $ createCheckpoint <$> eStore
       sequence $ createCheckpoint <$> eStore
       sequence $ createCheckpoint <$> eStore
--       sequence $ closeSimpleStore <$> eStore
--       eStore' <- openSimpleStore dir
       case eStore of
               (Left err) -> fail "Unable to open local state"
               (Right store) -> do
                     x' <- getSimpleStore store
                     if (x == x') 
                     then (runMemTestLoop (succ count) eStore dir x workingDir)
                     else fail "states are no longer equal"
