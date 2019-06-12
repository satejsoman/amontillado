{- 
  file      : SortedStringTable.hs 
  author    : satej soman (satej@uchicago.edu)
  copyright : none
  on-disk key-value store
-}

module SortedStringTable (
    SortedStringTable(..), 
    search
) where 

import KVEntry (Value(..), Key(..))
import Data.List.Split (splitOn)
import Data.Maybe
import System.IO
    
-- just a pointer to a file 
newtype SortedStringTable = SortedStringTable FilePath deriving (Eq, Show)

-- parse keys and values from lines on disk 
parseKV :: String -> (String, Int)
parseKV line = (key, value) where 
    (key:rawValue:_) = splitOn "," line
    value = read rawValue  

-- search table for key
search :: Key -> SortedStringTable -> IO (Maybe Int)
search (Key key) (SortedStringTable filePath) = do 
    -- fileHandle <- openFile filePath ReadWriteMode
    -- contents <- hGetContents fileHandle
    contents <- readFile filePath
    return (snd <$> listToMaybe (filter (\(k, _) -> k == key) (map parseKV (lines contents))))