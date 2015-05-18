{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module SimpleStore.IO (runStoreM, withReadLock, withWriteLock, readSimpleStore, writeSimpleStore, checkpointSimpleStore, openSimpleStore, makeSimpleStore, attemptOpenDefaultSimpleStore, closeSimpleStore) where

import           Control.Applicative
import           Control.Arrow (second)
import           Control.Concurrent.ReadWriteVar (RWVar)
import qualified Control.Concurrent.ReadWriteVar as RWVar
import           Control.Monad                   hiding (sequence)
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Class       (lift)
import           Data.Function
import           Data.List
import qualified Data.Serialize                  as S
import           Data.Text                       hiding (filter, foldl, map,
                                                  maximum, stripPrefix)
import           Data.Traversable
import           Filesystem
import           Filesystem.Path
import           Filesystem.Path.CurrentOS       hiding (decode)
import           Prelude                         hiding (FilePath, sequence,
                                                  writeFile)
import           SimpleStore.FileIO
import           SimpleStore.Internal
import           SimpleStore.Types
import Data.Either

runStoreM :: StoreM '[] a -> IO a
runStoreM storeAction = evalStateT (whileLocked storeAction) StoreNil

withReadLock :: SimpleStore st -> StoreM ('(ReadOnly, st) ': stack) a -> StoreM stack (Either StoreError a)
withReadLock store action = StoreM $ do
  outerStack <- get
  mResultAndStack <- lift $ RWVar.tryWith (simpleStoreVar store) $ \store -> runStateT (whileLocked action) (StoreCons store outerStack)
  maybe (return $ Left StoreLocked)
        (\(result, stack) -> do
           put $ restOf stack
           return $ Right result)
        mResultAndStack
  where
    restOf :: StoreStack ('(ReadOnly, st) ': stack) -> StoreStack stack
    restOf (StoreCons _ stack) = stack
 

withWriteLock :: SimpleStore st -> StoreM ('(ReadWrite, st) ': stack) a -> StoreM stack (Either StoreError a)
withWriteLock store action = StoreM $ do
  outerStack <- get
  mResultAndStack <- lift $ RWVar.tryModify (simpleStoreVar store) $ \store -> fmap (\(result, stack) -> (topOf stack, (result, stack))) $ runStateT (whileLocked action) (StoreCons store outerStack)
  maybe (return $ Left StoreLocked)
        (\(result, stack) -> do
           put $ restOf stack
           return $ Right result)
        mResultAndStack
  where
    topOf :: StoreStack ('(ReadWrite, st) ': stack) -> SimpleStoreRecord st
    topOf (StoreCons store _) = store
    
    restOf :: StoreStack ('(ReadWrite, st) ': stack) -> StoreStack stack
    restOf (StoreCons _ stack) = stack


withStoreIndex :: StoreIndex stack writeFlag a -> (SimpleStoreRecord a -> (b, SimpleStoreRecord a)) -> StoreStack stack -> (b, StoreStack stack)
withStoreIndex StoreHere f (StoreCons store stack) = let (x, newStore) = f store
                                                     in (x, StoreCons newStore stack)
withStoreIndex (StoreThere index) f (StoreCons store stack) = let (x, newStack) = withStoreIndex index f stack
                                                              in (x, StoreCons store newStack)

readSimpleStore :: StoreIndex stack writeFlag st -> StoreM stack st
readSimpleStore index = StoreM $ gets (fst . withStoreIndex index (\store -> (storeState store, store)))  

writeSimpleStore :: StoreIndex stack ReadWrite st -> st -> StoreM stack ()
writeSimpleStore index x = StoreM $ modify (snd . withStoreIndex index (\store -> ((), store { storeState = x })))

checkpointSimpleStore :: (S.Serialize st) => StoreIndex stack ReadWrite st -> StoreM stack (Either StoreError ())
checkpointSimpleStore index = StoreM $ do
  store <- gets (fst . withStoreIndex index (\store -> (store, store)))
  eNewStore <- lift $ checkpoint store
  either (return . Left) 
         (\store -> fmap Right $ modify (snd . withStoreIndex index (const ((), store))))
         eNewStore
  

-- | Open a simple store from a filepath reading in the newest most valid store
openSimpleStore :: S.Serialize st => FilePath -> IO (Either StoreError (SimpleStore st))
openSimpleStore fp = do
  dir <- makeAbsoluteFp fp
  exists <- isDirectory dir
  if exists
     then do attemptTakeLock fp >>= either
               (return . Left)
               (\_ -> 
                 do dirContents <- listDirectory dir
                    let files = filter isState dirContents
                    modifiedDates <-
                       traverse (\file -> do               -- Lambda is because the instance for Traversable on ()
                                    t <- getModified file  -- Traverses the second item so sequence only evaluates
                                    return (t,file)        -- the second item
                                   ) files
                    let sortedDates = snd <$> sortBy (compare `on` snd) modifiedDates
                    openNewestStore createStoreFromFilePath sortedDates)
     else return . Left $ StoreFolderNotFound

-- | Initialize a simple store from a given filepath and state.
-- The filepath should just be to the directory you want the state created in
-- as in "state"
makeSimpleStore :: (S.Serialize st) => FilePath -> st -> IO (Either StoreError (SimpleStore st))
makeSimpleStore dir state = do
  initializeDirectory dir >>= either 
    (return . Left)
    (\fp -> do
      let encodedState = S.encode state
          checkpointPath = fp </> (fromText . pack $ (show initialVersion) ++ "checkpoint.st")
          initialVersion = 0
      writeFile checkpointPath encodedState
      Right <$> createStore fp initialVersion state)


-- | Attempt to open a store. If the store doesn't it exist it will create the store in the filepath given
-- with makeSimpleStore.
attemptOpenDefaultSimpleStore :: (S.Serialize st) => FilePath -> st -> IO (Either StoreError (SimpleStore st))
attemptOpenDefaultSimpleStore fp initialState = do
  eStore <- openSimpleStore fp
  either (\_ -> makeSimpleStore fp initialState) (return . Right) eStore



-- | Release the file lock and close the handle to the file allowing another processes to open
-- the store
closeSimpleStore :: SimpleStore st -> IO ()
closeSimpleStore store = RWVar.with (simpleStoreVar store) $ \store -> do
  releaseFileLock store

