module Day8 where

import Data.List.Split (splitOn)
import Data.List (sort)
import qualified Data.Map as Map
import Data.Char (ord, digitToInt)
import Debug.Trace (trace)
import GHC.Unicode (isLower)
import Data.Map ((!))

type Point = (Int, Int)

run :: IO ()
run = do
    ls <- readFile "src/day8.txt"
    let treemap = Map.fromList $ parseMap [] ls 0 0
        visible = countVisible treemap
        viewingScore = maximum [scenicScore treemap p v | (p,v) <- Map.toList treemap]
    print $ "Day 8, Part 1: " ++ show visible
    print $ "Day 8, Part 2: " ++ show viewingScore
    print "Done"

scenicScore :: Map.Map Point Int -> Point -> Int -> Int
scenicScore m (x,y) hgt = up * right * down * left
    where 
        up = head [if Map.member (x,y-v) m then v else v - 1 | v <- [1..], isTaller (x,y-v)]
        down = head [if Map.member (x,y+v) m then v else v - 1 | v <- [1..], isTaller (x,y+v)]
        left = head [if Map.member (x-v,y) m then v else v - 1 | v <- [1..], isTaller (x-v,y)]
        right = head [if Map.member (x+v,y) m then v else v - 1 | v <- [1..], isTaller (x+v,y)]
        isTaller p = case Map.lookup p m of
          Nothing -> True
          Just n -> n >= hgt

countVisible :: Map.Map Point Int -> Int
countVisible m = length [v | (p,v) <- Map.toList m, isVisible m p v]

isVisible :: Map.Map Point Int -> Point -> Int -> Bool
isVisible m (x,y) hgt = null a || null b || null c || null d
    where
        a = [o | o <- [1..max], isTaller (x-o, y)]
        b = [o | o <- [1..max], isTaller (x+o, y)]
        c = [o | o <- [1..max], isTaller (x, y-o)]
        d = [o | o <- [1..max], isTaller (x, y+o)]
        max = maximum $ map (\(u,v) -> maximum [u,v]) $ Map.keys m
        isTaller p = case Map.lookup p m of
          Nothing -> False
          Just n -> n >= hgt

parseMap :: [(Point,Int)] -> [Char] -> Int -> Int ->  [(Point,Int)]
parseMap pts [] x y = pts
parseMap pts ('\n':xs) x y = parseMap pts xs 0 (y+1)
parseMap pts (h:xs) x y = parseMap (((x,y), digitToInt h):pts) xs (x+1) y
