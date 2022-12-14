module Day14 where

import Data.List.Split (splitOn)
import Data.List (sort)
import qualified Data.Set as Set
import Data.Char (ord, digitToInt)
import Debug.Trace (trace)
import GHC.Unicode (isLower)
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Sequence (sortBy)

type Point = (Int, Int)

run :: IO ()
run = do
    ls <- Map.fromList . concatMap (parseIntervals [] . splitOn " -> ") . lines <$> readFile "src/day14.txt"
    let maxY = maximum $ map snd $ Map.keys ls 
        (m, v) = dropSand maxY (500, 0) (500, 0) ls
        (m2, v2) = dropSandP2 maxY (500, 0) (500, 0) ls
        sandAtRest = [v | v<-Map.elems m, v == 'o']
        sandAtRestP2 = [v | v<-Map.elems m2, v == 'o']
    print $ "Day 14, Part 1: " ++ show (length sandAtRest)
    print $ "Day 14, Part 2: " ++ show (length sandAtRestP2)

dropSandP2 :: Int -> Point -> Point -> Map.Map Point Char -> (Map.Map Point Char, Bool)
dropSandP2 maxY origin (x,y) m 
  | Map.member origin m = (m, True)
  | not (Map.member (x,y+1) m) && not (isFloor (x,y+1)) = dropSandP2 maxY origin (x,y+1) m
  | not (Map.member (x-1,y+1) m) && not (isFloor (x-1,y+1)) = dropSandP2 maxY origin (x-1,y+1) m
  | not (Map.member (x+1,y+1) m) && not (isFloor (x+1,y+1)) = dropSandP2 maxY origin (x+1,y+1) m
  | otherwise = dropSandP2 maxY origin origin (Map.insert (x,y) 'o' m)
    where 
      isFloor (x,y) = y == maxY + 2

dropSand :: Int -> Point -> Point -> Map.Map Point Char -> (Map.Map Point Char, Bool)
dropSand maxY origin (x,y) m 
  | y > maxY = (m, True)
  | not (Map.member (x,y+1) m) = dropSand maxY origin (x,y+1) m
  | not (Map.member (x-1,y+1) m) = dropSand maxY origin (x-1,y+1) m
  | not (Map.member (x+1,y+1) m) = dropSand maxY origin (x+1,y+1) m
  | otherwise = dropSand maxY origin origin (Map.insert (x,y) 'o' m)

parseIntervals :: [(Point, Char)] -> [String] -> [(Point, Char)]
parseIntervals l [] = error "Unknown interval"
parseIntervals l [y] = l
parseIntervals l (a:b:xs)
  | ax == bx && ay < by = parseIntervals (l ++ [((ax,y),'#') | y <- [ay..by]]) (b:xs)
  | ax == bx && ay > by = parseIntervals (l ++ [((ax,y),'#') | y <- [by..ay]]) (b:xs)
  | ax < bx && ay == by = parseIntervals (l ++ [((x,ay),'#') | x <- [ax..bx]]) (b:xs)
  | ax > bx && ay == by = parseIntervals (l ++ [((x,ay),'#') | x <- [bx..ax]]) (b:xs)
  | otherwise = error "Invalid interval"
  where
      [ax, ay] = map (\ k -> read k :: Int) (splitOn "," a)
      [bx, by] = map (\ k -> read k :: Int) (splitOn "," b)




