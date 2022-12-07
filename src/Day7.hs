module Day7 where

import Data.List.Split (splitOn)
import Data.List (sort)
import qualified Data.Map as Map
import Data.Char (ord)
import Debug.Trace (trace)
import GHC.Unicode (isLower)
import Data.Map ((!))

type Path = [String]

run :: IO ()
run = do
    ls <- readFile "src/day7.txt"
    let cmds = tail $ map lines (splitOn "$ " ls)
        parsedPaths = parseCmds cmds Map.empty []
        sizes = mapSizes parsedPaths
        sumDirs = sum [v | (k,v) <- sizes, v <= 100000]
        reqFreedSpace = Map.fromList sizes ! ["/"] - 40000000
        smallestDirToDelete = minimum [v | (k,v) <- sizes, v >= reqFreedSpace]
    print $ "Day 7, Part 1: " ++ show sumDirs
    print $ "Day 7, Part 2: " ++ show smallestDirToDelete
    print "Done"

mapSizes :: Map.Map Path [String] -> [(Path, Int)]
mapSizes m = [(p, sum $ map (mapDir p) v) | (p,v) <- Map.toList m]
    where
        mapDir p x = let [st, en] = splitOn " " x in
            if st /= "dir" then read st :: Int
            else sum $ map (mapDir (en:p)) (m ! (en:p))

parseCmds :: [[String]] -> Map.Map Path [String] -> Path -> Map.Map Path [String]
parseCmds [] m p = m
parseCmds (x:xs) m p
    | head x == "ls" = parseCmds xs (Map.insert p (tail x) m) p
    | let [cmd, v] = splitOn " " (head x) in cmd == "cd" = let [cmd, v] = splitOn " " (head x) in parseCmds xs m (if v == ".." then tail p else v:p)
    | otherwise = error $ "Unknown command " ++ head x
