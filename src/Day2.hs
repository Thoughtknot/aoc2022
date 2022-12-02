module Day2 where

import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.List (sort)

type Match = (String, String)

run :: IO ()
run = do
    ls <- readFile "src/day2.txt"
    let matches = map parseMatch $ lines ls
        score = runMatch matches 0
        score2 = runMatchP2 matches 0
    print $ "Day 2, Part 1: " ++ show score
    print $ "Day 2, Part 2: " ++ show score2
    print "Done"

runMatchP2 :: [Match] -> Int -> Int
runMatchP2 [] s = s
runMatchP2 (x:xs) s = runMatchP2 xs (s+ns) 
    where
        ns = case x of
            ("A", "X") -> 3 + 0
            ("A", "Y") -> 1 + 3
            ("A", "Z") -> 2 + 6
            ("B", "X") -> 1 + 0
            ("B", "Y") -> 2 + 3
            ("B", "Z") -> 3 + 6
            ("C", "X") -> 2 + 0
            ("C", "Y") -> 3 + 3
            ("C", "Z") -> 1 + 6
            _ -> error "Impossible"

runMatch :: [Match] -> Int -> Int
runMatch [] s = s
runMatch (x:xs) s = runMatch xs (s+ns) 
    where
        ns = case x of
            ("A", "X") -> 1 + 3
            ("A", "Y") -> 2 + 6
            ("A", "Z") -> 3 + 0
            ("B", "X") -> 1 + 0
            ("B", "Y") -> 2 + 3
            ("B", "Z") -> 3 + 6
            ("C", "X") -> 1 + 6
            ("C", "Y") -> 2 + 0
            ("C", "Z") -> 3 + 3
            _ -> error "Impossible"

parseMatch :: String -> Match
parseMatch line = let [a,b] = splitOn " " line in (a,b)