# simple-cell

Simple Store  takes a filename generating function and a SimpleStore
it then generates the machinery to handle the creation of multiple atomic values of this type according to the unique keys of each one.


## Usage

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
    acell <- makeCellSomeStoreState
    drId  <- insertState acell SomeStoreState
    rslt  <- queryCell acell allConsistentStates 
    ast   <- getState acell drId
    deleteState drId 
    

```

a *DirectedKeyRaw* is a key with a source and a destination.  The source and destination are very arbitrary but were
created to deal with database keys coming from multiple locations.  

