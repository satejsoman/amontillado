{- 
  file      : MemoryTable.hs 
  author    : satej soman (satej@uchicago.edu)
  copyright : none
  in-memory key-value store backed by a red-black tree
-}

module MemoryTable (
    MemoryTable(..),
    insert, 
    search
) where 

import KVEntry (Key(..), Value(..), Entry(..))
import Data.Foldable (toList)

data Color = Red | Black deriving (Eq, Show)

data MemoryTable a = KVNode { entry  :: a, lChild :: MemoryTable a,  rChild :: MemoryTable a, color :: Color } 
                   | Empty 
                   deriving (Eq)

-- search Tree for key
search :: Key -> MemoryTable Entry -> Maybe Value
search _ Empty = Nothing 
search key (KVNode (Entry k v _) left right _)
    | (<)  key k = search key left
    | (>)  key k = search key right
    | (==) key k = Just v
    | otherwise  = Nothing

-- add (k, v) pair to tree and maintain invariants
insert :: (Show a, Ord a) => a -> MemoryTable a -> MemoryTable a
insert entry Empty = KVNode entry Empty Empty Red
insert entry tree  = KVNode newEntry leftChild rightChild Black where
    balancedInsert Empty = KVNode entry Empty Empty Red 
    balancedInsert t@(KVNode rootEntry left right clr) 
        | entry < rootEntry = balance (KVNode rootEntry (balancedInsert left) right clr)  
        | entry > rootEntry = balance (KVNode rootEntry left (balancedInsert right) clr)  
        | otherwise = t 
    (KVNode newEntry leftChild rightChild _) =  balancedInsert tree

-- maintains red-black invariants: 
--  1. no red node has a red child
--  2. every path from root to leaf contains the same number of black nodes
balance :: MemoryTable a -> MemoryTable a
balance (KVNode z (KVNode x a (KVNode y b c Red) Red) d Black) = KVNode y (KVNode x a b Black) (KVNode z c d Black) Red
balance (KVNode z (KVNode y (KVNode x a b Red) c Red) d Black) = KVNode y (KVNode x a b Black) (KVNode z c d Black) Red
balance (KVNode x a (KVNode y b (KVNode z c d Red) Red) Black) = KVNode y (KVNode x a b Black) (KVNode z c d Black) Red
balance (KVNode x a (KVNode z (KVNode y b c Red) d Red) Black) = KVNode y (KVNode x a b Black) (KVNode z c d Black) Red
balance node@KVNode{} = node
balance Empty = Empty

-- merge two trees together ; balancing should allow for associativity
merge :: (Show a, Ord a) => MemoryTable a -> MemoryTable a -> MemoryTable a 
merge tree Empty = tree 
merge Empty tree = tree 
merge (KVNode kv left right _) tree = insert (entry right) (insert kv (insert (entry left) tree))

-- color representation of nodes based on color bit 
coloredParens :: Color -> (String, String)
coloredParens Black = ("\x1b[30m(\x1b[0m", "\x1b[30m)\x1b[0m")
coloredParens Red   = ("\x1b[31m(\x1b[0m", "\x1b[31m)\x1b[0m")

instance (Show a) => Show (MemoryTable a) where 
    show Empty = "-"
    show (KVNode entry lChild rChild color) = lParen ++ show entry ++ " " ++ show lChild ++ " " ++ show rChild ++ rParen where 
        (lParen, rParen) = coloredParens color

instance (Show a, Ord a) => Semigroup (MemoryTable a) where 
    (<>) = merge
instance (Show a, Ord a) => Monoid (MemoryTable a) where 
    mempty = Empty
    
instance Foldable MemoryTable where 
    foldr _ acc Empty = acc
    foldr f acc (KVNode kv left right _) = foldr f (f kv (foldr f acc right)) left

main :: IO ()
main = do
    putStrLn "initial tree: "
    let tree  = insert (Entry (Key "z") (Value 0) 0) Empty
    print tree
    let tree2 = insert (Entry (Key "a") (Value 1) 0) tree
    print tree2
    let tree3 = insert (Entry (Key "bob") (Value 1000) 0) tree2
    print tree3
    let tree4 = insert (Entry (Key "foo") (Value 10) 0) tree3
    print tree4
    let tree5 = insert (Entry (Key "bar") (Value 11) 0) tree4
    print tree5

    putStrLn "\nprint each element of the final tree: "
    sequence_ [putStrLn (" - " ++ show each) | each <- toList tree5]
