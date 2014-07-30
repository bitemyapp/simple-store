# simple-cell

<<<<<<< HEAD
Often in haskell you have a shared data type that needs persistence, atomicity and consistency.
=======
Simple Store  takes a filename generating function and a SimpleStore
it then generates the machinery to handle the creation of multiple atomic values of this type according to the unique keys of each one.
>>>>>>> 65d75a5aa525f2ac7b6b4ad24e2d3c12e0b80361


## Usage

<<<<<<< HEAD
The core datatype for SimpleStore is

``` haskell

data SimpleStore st = SimpleStore {
    storeFP     :: FilePath
  , storeState  :: TVar st
  , storeLock   :: TMVar StoreLock
  , storeHandle :: Maybe Handle
}


```

Say your Datatype is :

``` haskell

data Address = Address { street :: Street , town::Town}
  deriving (Generic,Eq,Ord)

instance Serialize Address where 


``` haskell

You might write:

``` haskell
-- | makeSimpleStore :: FilePath -> st -> IO (Either StoreError (SimpleStore st))

djangosAddress = do
   eSt(makeSimpleStore "DjangoAddress" (Address MinorSwingBlvd GypsyTown))
   either (\e -> fail (show e) ) (return st)  eSt

=======
### Inputs ...

AcidCells are designed to take a function to retrieve keys and a function to make those keys into filenames

The output of the functions should be unique *AcidCell* does not check this for you

``` haskell


-- given

-- The user supplies a variable with an instance of Serializeable defined

myAcidStateVariable = x :: SomeSerializableThing


-- define

myRootDir :: FilePath
myRootDir = "./stateSpace"


-- Fill this guy
data CellKey k h s t st = CellKey { getKey :: st -> (DirectedKeyRaw k h s t)
                                  , codeCellKeyFilename :: (DirectedKeyRaw k h s t) -> Text
                                  , decodeCellKeyFilename :: Text -> (DirectedKeyRaw k h s t)
                                  }
                    

$(makeStoreCell `myCellKey 'initialState ''SomeSerializeableThing )

```


### Creates ...

Here is what the template haskell call above generates.

``` haskell

-- data types for managing states in the cell both active and dormant
data CellCore k h s t stlive stdormant = CellCore { 
      ccLive :: (M.Map (DirectedKeyRaw k h s t) stlive )
      ccDormant :: (M.Map (DirectedKeyRaw k h s t) stdormant )
    }

type TCellCore k h s t stlive stdormant = TVar (CellCore k h s t (TVar stlive) (TVar stdormant))

-- Generate dig for CellCoreDormant
-- These are not directly availalbe to the user
insertCellSomeStorePath :: CellCore -> SomeStoreState -> Update ...
deleteCellSomeStorePath :: CellCore -> SomeStoreState -> Update ...
getCellSomeStorePath    :: CellCore -> SomeStoreState -> Query ...   

-- DIG structure 
data CellCore  k src dst tm tvlive stdormant = CellCore { 
       ccLive     :: TVar (M.Map (DirectedKeyRaw  k src dst tm) tvlive )
      ,ccDormant :: stdormant
    }


-- UI Functions
insertState :: StoreCell -> <SomeStoreState> -> IO (EventResult InsertStoreCellPathFileKey)
deleteState :: StoreCell -> DirectedKeyRaw -> IO Bool
getState      :: StoreCell -> DirectedKeyRaw -> IO (Either StoreCellError SomeStoreState)
queryCell   :: StoreCell -> (SomeStoreState -> a ) -> IO (Either StoreCellError (monoid a))

-- updateState
-- deleteWhere


type StoreCellSomeStoreState = StoreCell SomeStoreState


makeCellSomeStoreState :: IO StoreCellSomeStoreState

``` 

### Use

``` haskell

someStateManip :: IO ()
n    acell <- makeCellSomeStoreState
    drId  <- insertState acell SomeStoreState
    rslt  <- queryCell acell allConsistentStates 
    ast   <- getState acell drId
    deleteState drId 
    
>>>>>>> 65d75a5aa525f2ac7b6b4ad24e2d3c12e0b80361

```
A directory containing a serialized version of Django's address is saved for you
**Note** you can use text serialization too so it is readable.


Now you can share that between processes (in a Yesod for instance)

<<<<<<< HEAD


lets walk there
``` haskell


walkToDjangosHouse :: IO () 
walkToDjangosHouse = do
  djAddr <- getSimpleStore djangosAddress
  walkToDjangosHouse djAddr 

```
=======
a *DirectedKeyRaw* is a key with a source and a destination.  The source and destination are very arbitrary but were
created to deal with database keys coming from multiple locations.  

>>>>>>> 65d75a5aa525f2ac7b6b4ad24e2d3c12e0b80361
