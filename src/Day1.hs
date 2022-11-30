module Day1 where

import Data.List.Split (splitOn)
import qualified Data.Map as Map

type Point = (Int, Int)

run :: IO ()
run = do
    ls <- readFile "src/day1.txt"
    print $ "Day 1, Part 1: " ++ show 1
    print $ "Day 1, Part 2: " ++ show 2
    print "Done"