{- 
  file      : KVEntry.hs 
  author    : satej soman (satej@uchicago.edu)
  copyright : none
  data types for key value pairs 
-}

module KVEntry (
    Key(..), 
    Value(..), 
    Timestamp, 
    Entry(..)
) where 

newtype Key = Key String deriving (Eq, Show, Ord)
data Value = Value Int -- current value
           | Tombstone -- deleted value
           deriving (Eq, Show)

type Timestamp = Int 
data Entry = Entry Key Value Timestamp deriving (Eq)

instance Show Entry where 
    show (Entry (Key k) (Value v) _) = "[" ++ k ++ ": " ++ show v ++ "]"

instance Ord Entry where 
    (<=) (Entry k1 _ _) (Entry k2 _ _) = (<=) k1 k2