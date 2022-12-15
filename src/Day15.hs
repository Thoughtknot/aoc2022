{-# LANGUAGE InstanceSigs #-}
module Day15 where

import Data.List.Split (splitOn)
import Data.List (sort)
import qualified Data.Set as Set
import Data.Char (ord, digitToInt)
import Debug.Trace (trace)
import GHC.Unicode (isLower)
import Data.Map ((!))
import qualified Data.Map as Map

data Interval = Interval (Int, Int) deriving (Eq, Show)

instance Ord Interval where
  compare (Interval (as,ae)) (Interval (ba,be)) = compare as ba

type Point = (Int, Int)
type Beacon = Point
type Sensor = (Point, Beacon, Int)

run :: IO ()
run = do
    ls <- map parseSensor . lines <$> readFile "src/day15.txt"
    let val = [(getInterval 2000000 v, v) | v<-ls]
        sortedIntervals = sort $ map fst val
        merged = mergeInterval sortedIntervals []
        size = sum [y-x | Interval (x,y) <- merged]
    print $ "Day 15, Part 1: " ++ show size
    let p2 = [(y, mergeInterval (sort [getInterval y v | v<-ls]) []) | y<-[0..4000000]]
        (y, ints) = head [(y,ints) | (y,ints)<-p2, not (containsInterval (Interval (0,4000000)) ints)]
        x = head [xv | xv <- [0..4000000], not (inIntervals xv ints)]
    print $ "Day 15, Part 2: " ++ show (x * 4000000 + y)

inIntervals :: Int -> [Interval] -> Bool
inIntervals x ints = not $ null [True | (Interval (a,b))<-ints, a <= x && b >= x]

containsInterval :: Interval -> [Interval] -> Bool
containsInterval (Interval (x,y)) ints = not (null [True | (Interval (a,b))<-ints, x >= a && y <= b])

mergeInterval :: [Interval] -> [Interval] -> [Interval]
mergeInterval (h:t) [] = mergeInterval t [h]
mergeInterval [] mrg = mrg
mergeInterval (h@(Interval (hs,he)):t) v@(((Interval (fs,fe))):nt) =
  if hs > fe then
    mergeInterval t (h:v)
  else
    mergeInterval t (Interval (fs,ne):nt)
  where
    ne = max fe he

getInterval :: Int -> Sensor -> Interval
getInterval row ((x,y),(bx,by),r)
  | abs (y-row) > r = Interval (0, 0)
  | otherwise = Interval (is,ie)
    where
      d = r - abs (y-row)
      is = x-d
      ie = x+d

distance :: Point -> Point -> Int
distance (ax, ay) (bx, by) = abs (ax-bx) + abs (ay-by)

parseSensor :: String -> Sensor
parseSensor v = ((sx,sy), (bx, by), r)
  where
    [_, _, sxf, syf, _, _, _, _, bxf, byf] = words v
    sx = read (drop 2 (init sxf)) :: Int
    sy = read (drop 2 (init syf)) :: Int
    bx = read (drop 2 (init bxf)) :: Int
    by = read (drop 2 byf) :: Int
    r = distance (sx,sy) (bx, by)
