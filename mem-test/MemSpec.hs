{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}



-- import           Control.Concurrent

import GHC.Generics 
import Data.Serialize
-- import           Control.Concurrent.STM.TMVar
import Control.Concurrent (threadDelay)
import           Data.Either
import           Data.Traversable
-- import           Data.Traversable
import Control.Applicative
import           Filesystem
import           Filesystem.Path
import           Filesystem.Path.CurrentOS
import           Prelude                      hiding (sequence)
import           SimpleStore



newtype TestNum = TestNum { unNum::Int}
 deriving (Generic,Show,Eq)

instance Serialize TestNum where 


intList :: [TestNum]
intList = TestNum <$> [0 .. 1000]

main :: IO () 
main = do dataSet <- traverse makeTestStores  intList
          putStrLn "starting memory loop"
          _ <- traverse (runMemTestLoop 0 ) dataSet
          return () 

loopMax :: Int
loopMax = 1*1000*1000

runMemTestLoop count (eStore ,dir ,x ,workingDir )
  | count > loopMax = (removeTree $ workingDir </> dir) >>
                      return () 
  | otherwise = do 
       threadDelay (1*100)
       st <- traverse getSimpleStore eStore
       print st
       case eStore of
               (Left err) -> fail "Unable to open local state"
               (Right store) -> do
                     x' <- getSimpleStore store
                     eT <- modifySimpleStore store (return)
                     print eT
                     if (x == x')                     
                     then (runMemTestLoop (succ count) (eStore ,dir ,x' ,workingDir))
                     else fail "states are no longer equal"


makeTestStores i = do 
   let dir = Filesystem.Path.concat ["xkcd-test-states" , decodeString.show $ i]
   workingDir <- getWorkingDirectory
   eStore <- makeSimpleStore dir i 
   return (eStore,dir,i,workingDir)
