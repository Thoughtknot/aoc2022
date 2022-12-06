module Day6 where

import Data.List.Split (splitOn)
import Data.List (sort)
import qualified Data.Set as Set
import Data.Char (ord)
import Debug.Trace (trace)
import GHC.Unicode (isLower)
import Data.Map ((!))

run :: IO ()
run = do
    ls <- readFile "src/day6.txt"
    let (marker, prevl) = findSoPMarker ls []
        (marker2, prevl2) = findSoMMarker ls []
    print $ "Day 6, Part 1: " ++ show (length prevl)
    print $ "Day 6, Part 2: " ++ show (length prevl2)
    print "Done"

findSoMMarker :: [Char] -> [Char] -> (Char, [Char])
findSoMMarker [] s = ('0', s)
findSoMMarker (x:xs) v
    | Set.size (Set.fromList $ take 14 v) == 14 = (x,v)
    | otherwise = findSoMMarker xs (x:v)

findSoPMarker :: [Char] -> [Char] -> (Char, [Char])
findSoPMarker [] s = ('0', s)
findSoPMarker (x:xs) v
    | Set.size (Set.fromList $ take 4 v) == 4 = (x,v)
    | otherwise = findSoPMarker xs (x:v)