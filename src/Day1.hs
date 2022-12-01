module Day1 where

import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.List (sort)

type Point = (Int, Int)

run :: IO ()
run = do
    ls <- readFile "src/day1.txt"
    let elves = map (map (\y -> read y :: Int) . lines) $ splitOn "\n\n" ls
        sumPerElf = map sum elves
        topThreeSum = sum $ take 3 $ reverse $ sort sumPerElf
    print $ "Day 1, Part 1: " ++ show (maximum sumPerElf)
    print $ "Day 1, Part 2: " ++ show topThreeSum
    print "Done"