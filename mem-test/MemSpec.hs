{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}



-- import           Control.Concurrent

import GHC.Generics 
import Data.Serialize
import Control.Monad
-- import           Control.Concurrent.STM.TMVarp
import Control.Concurrent (threadDelay, forkIO)
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
 deriving (Generic,Show,Eq,Num,Integral,Real,Enum,Ord)

instance Serialize TestNum where 


intList :: [TestNum]
intList = TestNum <$> [0 .. 1000]

main :: IO () 
main = do dataSet <- traverse makeTestStores  intList
          putStrLn "starting memory loop"
          _ <- traverse (\l -> forkIO (runMemTestLoop l ) ) dataSet           
          removeTree "xkcd-test-states"
          return ()


runMemTestLoop  (eStore ,dir ,x ,workingDir ) = do 
       threadDelay (1*1000*1000)
       st <- traverse (\store -> runStoreM $ withReadLock store $ readSimpleStore StoreHere) eStore
       case eStore of
               (Left err) -> fail "Unable to open local state"
               (Right store) -> do
                     (Right x') <- runStoreM $ withReadLock store $ readSimpleStore StoreHere
                     eT <- runStoreM $ withWriteLock store $ do
                       x <- readSimpleStore StoreHere
                       writeSimpleStore StoreHere (mod (x + 1) 10000)
                       checkpointSimpleStore StoreHere
                     print x'
                     runMemTestLoop (eStore ,dir ,x' ,workingDir)


makeTestStores i = do 
   createDirectory True "xkcd-test-states"
   let dir = Filesystem.Path.concat ["xkcd-test-states" , decodeString.show $ i]
   workingDir <- getWorkingDirectory
   eStore <- makeSimpleStore dir i 
   return (eStore,dir,i,workingDir)
