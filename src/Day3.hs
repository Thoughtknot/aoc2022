module Day3 where

import Data.List.Split (splitOn)
import Data.List (sort)
import qualified Data.Set as Set
import Data.Char (ord)
import Debug.Trace (trace)
import GHC.Unicode (isLower)

type Rucksack = ([Char], [Char])

run :: IO ()
run = do
    ls <- lines <$> readFile "src/day3.txt"
    let k = map (\x -> splitAt (length x `div` 2) x) ls
        commonLetterPriorities = map getPriority $ findCommonItems k []
        badges = findBadge (group 3 ls) []
        badgePriorities = map getPriority badges
    print $ "Day 3, Part 1: " ++ show (sum commonLetterPriorities)
    print $ "Day 3, Part 2: " ++ show (sum badgePriorities)
    print "Done"

findBadge :: [[String]] -> [Char] -> [Char]
findBadge [] cs = cs
findBadge (x:xs) cs = findBadge xs (nv:cs)
    where
        f = map Set.fromList x
        nv = head $ Set.toList (intersection f (head f))
        
intersection ::  [Set.Set Char] -> Set.Set Char -> Set.Set Char
intersection [] s = s
intersection (x:xs) s = intersection xs (Set.intersection s x)

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = take n l : group n (drop n l)
  | otherwise = error "Negative or zero n"

getPriority :: Char -> Int
getPriority a = if isLower a then ord a - ord 'a' + 1 else ord a - ord 'A' + 27

findCommonItems :: [Rucksack] -> [Char] -> [Char]
findCommonItems [] ls = ls
findCommonItems ((l,r):xs) ls = findCommonItems xs (nc : ls)
    where
        letter = Set.intersection (Set.fromList l) (Set.fromList r)
        nc = head $ Set.toList letter
