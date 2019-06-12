{- 
  file      : LSMTree.hs 
  author    : satej soman (satej@uchicago.edu)
  copyright : none
  log-structured merge tree 
-}

module LSMTree where 

import KVEntry
import MemoryTable (MemoryTable(..), insert, search)
import SortedStringTable (SortedStringTable(..), search)
import Data.Foldable (toList)

defaultThreshold :: Int
defaultThreshold = 1000


data LSMTree = LSMTree { 
    size      :: Int,
    threshold :: Int, 
    memTable  :: MemoryTable Entry,
    ssTable   :: SortedStringTable
} --deriving (Eq, Show)

-- empty tree with default threshold
makeTree :: FilePath -> LSMTree 
makeTree path = LSMTree 0 defaultThreshold Empty (SortedStringTable path)

-- empty tree with custom threshold
makeTreeWithThreshold :: FilePath -> Int -> LSMTree 
makeTreeWithThreshold path threshold = LSMTree 0 threshold Empty (SortedStringTable path)

-- insert a (k, v) pair into the LSM tree and compact if it pushes past in-memory capacity 
put :: (String, Int) -> LSMTree -> LSMTree
put (k, v) (LSMTree size threshold memTable ssTable) 
    | size + 1 > threshold = LSMTree 0 threshold Empty ssTable
    | otherwise = LSMTree (size + 1) threshold (insert (Entry (Key k) (Value v) 0) memTable) ssTable

-- look up key in memory and on disk 
get :: String -> LSMTree -> IO (Maybe Int)
get searchKey (LSMTree _ _ memTable ssTable) = do 
    let key = Key searchKey
    case MemoryTable.search key memTable of 
        Nothing -> SortedStringTable.search key ssTable
        (Just (Value v)) -> return (Just v)
