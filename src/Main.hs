module Main where

import LSMTree
import MemoryTable
import SortedStringTable
import KVEntry
import System.Environment 
import System.Exit
import Data.Foldable (toList)

usage :: IO a
usage = putStrLn usageMessage >> exitWith (ExitFailure 1) where 
    usageMessage = "usage: Main.hs --path <path to data file for serializing SortedStringTable>"

updateOnDisk :: LSMTree -> IO LSMTree
updateOnDisk (LSMTree _ threshold memTable (SortedStringTable path)) = do 
    let newPath = "new" ++ path
    oldWrites <- readFile path
    appendFile newPath oldWrites
    appendFile newPath (unlines (map show (toList memTable)))
    return (LSMTree 0 threshold Empty (SortedStringTable newPath))

putAndCompact :: (String, Int) -> LSMTree -> IO LSMTree 
putAndCompact (k, v) tree@(LSMTree size threshold memTable ssTable) = if size < threshold 
    then return (LSMTree (size + 1) threshold (insert (Entry (Key k) (Value v) 0) memTable) ssTable)
    else updateOnDisk (LSMTree size threshold (insert (Entry (Key k) (Value v) 0) memTable) ssTable)

runSmokeTest :: FilePath -> IO ()
runSmokeTest path = do
    let lsmTree = makeTreeWithThreshold path 3
    putStrLn ("We've read in data from " ++ path ++ ". We can run some sample queries on data in the on-disk table.")
    putStrLn "--> GET 'apple' (should be Just 3) (press enter to continue)" >> getLine
    lookup1 <- get "apple" lsmTree
    putStrLn ("<-- " ++ show lookup1)

    putStrLn "--> GET 'nonexistentkey' (should be Nothing) (press enter to continue)" >> getLine
    lookup2 <- get "nonexistentkey" lsmTree
    putStrLn ("<-- " ++ show lookup2)
    

    putStrLn "\n\nOur in-memory table has a threshold of 3. Let's add 4 writes and ensure the table is flushed."
    putStrLn "--> PUT 'cat' 0" 
    putStrLn "--> PUT 'dog' 1 " 
    putStrLn "--> PUT 'book' 1000"
    lsmTree1 <- putAndCompact ("cat", 0) lsmTree
    lsmTree2 <- putAndCompact ("dog", 1) lsmTree1
    lsmTree3 <- putAndCompact ("book", 1000) lsmTree2
    putStrLn ("This is what the state of the in-memory tree looks like after 3 writes: \n" ++ show (memTable lsmTree3))
    putStrLn "--> PUT 'icecream' 200 (press enter to continue)" >> getLine 

    lsmTree4 <- putAndCompact ("icecream", 200) lsmTree3
    putStrLn ("This is what the state of the in-memory tree looks like after 4 writes: \n" ++ show (memTable lsmTree4))
    print (memTable lsmTree3)

    putStrLn "The new on-disk table (new" ++ path ++ ") has the new writes appended."

    return ()
    

main :: IO ()
main = do 
    args <- getArgs
    case args of 
        ["--data", path] -> runSmokeTest path
        _                -> usage