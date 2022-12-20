module Day17 where

import Data.List.Split (splitOn)
import Data.List (sort)
import qualified Data.Set as Set
import Data.Char (ord, digitToInt)
import Debug.Trace (trace)
import GHC.Unicode (isLower)
import Data.Map ((!))
import qualified Data.Map as Map

type Point = (Int, Int)
type Shape = [Point]

type Graph = Map.Map Point Char

run :: IO ()
run = do
    jetPattern <- readFile "src/day17.txt"
    let mvp = movePieces (Map.fromList [((x,0),'-')|x<-[0..6]], 0) (length jetPattern) (cycle jetPattern) Map.empty 0 2022
    print $ "Day 17, Part 1: " ++ show mvp
    let mvp2 = movePieces (Map.fromList [((x,0),'-')|x<-[0..6]], 0) (length jetPattern) (cycle jetPattern) Map.empty 0 1000000000000
    print $ "Day 17, Part 2: " ++ show mvp2

movePieces :: (Graph, Int) -> Int -> [Char] -> Map.Map Point Point -> Int -> Int -> Int
movePieces (g,v) njs js cache i max
  | i == max = height
  | isCached = cv
  | otherwise = movePieces (movePiece g v (drop v js) (getObj i g)) njs js ncache (i + 1) max
    where 
      height = maximum [ky | (kx,ky)<-Map.keys g]
      ck = (i `mod` 5, v `mod` njs)
      (isCached, cv) = case Map.lookup ck cache of
        Nothing -> (False, 0)
        Just (x,y) -> if y > 0 && (max - i) `mod` (i - x) == 0 then 
            (True, height + (((max - i) `div` (i - x)) * (height - y)))
          else
            (False, 0)
      ncache = Map.insert ck (i, height) cache

movePiece :: Graph -> Int -> [Char] -> Shape -> (Graph, Int)
movePiece g v [] sh = error "Jet streams should repeat"
movePiece g v (x:xs) sh = do
  let ns = if x == '<' then moveShape sh (-1,0)
           else moveShape sh (1,0)
      ss = if collides ns g then sh else ns
      nsh = moveShape ss (0,-1)
      mv = Map.union g $ Map.fromList [(v, '#')| v <- ss]
  if collides nsh g then
    (mv,v+1)
  else
    movePiece g (v+1) xs nsh

moveShape :: Shape -> (Int, Int) -> Shape
moveShape s (dx,dy) = [(x+dx,y+dy) | (x,y)<-s]

collides :: Shape -> Graph -> Bool
collides s g = not $ null [v | v@(x,y)<-s, Map.member v g || x > 6 || x < 0]

getObj :: Int -> Graph -> Shape
getObj j g
  | i == 0 = [(x+2,y)|x<-[0..3]] --dash
  | i == 1 = (3,y):(3,y+2):[(x+2,y+1)|x<-[0..2]] -- Plus
  | i == 2 = (4,y+2):(4,y+1):[(x+2,y)|x<-[0..2]] -- L
  | i == 3 = [(2,y+a)|a<-[0..3]] -- I
  | i == 4 = [(x+2,y)|x<-[0..1]] ++ [(x+2,y+1)|x<-[0..1]] -- square
  |otherwise = error "Unknown object"
  where
    i = j `mod` 5
    y = maximum [ky | (kx,ky)<-Map.keys g] + 4

toString :: Graph -> String
toString g = concat [toStr (x,y)|y<-reverse[0..maxy],x<-[0..6]]
  where
    maxy = maximum [ky | (kx,ky)<-Map.keys g]
    toStr (a,b) = if a == 6 then Map.findWithDefault '.' (a,b) g:"\n" else [Map.findWithDefault '.' (a,b) g]