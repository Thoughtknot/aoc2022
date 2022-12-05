module Day4 where

import Data.List.Split (splitOn)
import Data.List (sort)
import qualified Data.Set as Set
import Data.Char (ord)
import Debug.Trace (trace)
import GHC.Unicode (isLower)

run :: IO ()
run = do
    ls <- lines <$> readFile "src/day4.txt"
    let parsedPairs = map parsePair ls
        fullyContainedPairs = [x | x <- parsedPairs, fullyContains x]
        anyOverlapPairs = [x | x <- parsedPairs, anyOverlap x]
    print $ "Day 4, Part 1: " ++ show (length fullyContainedPairs)
    print $ "Day 4, Part 2: " ++ show (length anyOverlapPairs)
    print "Done"

anyOverlap ::  (Set.Set Int, Set.Set Int) -> Bool
anyOverlap (a,b) = Set.size (Set.intersection a b) > 0

fullyContains ::  (Set.Set Int, Set.Set Int) -> Bool
fullyContains (a,b) = let u = Set.union a b in u == a || u == b

parsePair :: String -> (Set.Set Int, Set.Set Int)
parsePair line = (Set.fromList [fmin..fmax], Set.fromList [smin..smax])
    where
        [fe, se] = splitOn "," line
        [fmin, fmax] = map (\x -> read x :: Int) $ splitOn "-" fe
        [smin, smax] = map (\x -> read x :: Int) $ splitOn "-" se