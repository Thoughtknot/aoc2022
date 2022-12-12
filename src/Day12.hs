module Day12 where

import Data.List.Split (splitOn)
import Data.List (sort)
import qualified Data.Set as Set
import Data.Char (ord, digitToInt)
import Debug.Trace (trace)
import GHC.Unicode (isLower)
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Function (on)
import Data.Foldable (minimumBy)

type Point = (Int, Int)

type M = Map.Map Point Char
type D = Map.Map Point Int
type C = Map.Map Point Point

run :: IO ()
run = do
    mp <- parseMap Map.empty 0 0 <$> readFile "src/day12.txt"
    let start = head [k | (k,v)<-Map.toList mp, v == 'S']
        end = head [k | (k,v)<-Map.toList mp, v == 'E']
        cf = aStar start end [start] (mp, Map.singleton start 0, Map.singleton start (distance start end), Map.empty)
        path = getPath [end] cf end start
        cfsP2 = [(k, aStar k end [k] (mp, Map.singleton k 0, Map.singleton k (distance k end), Map.empty)) | (k,v)<-Map.toList mp, v == 'a' || v == 'S']
        cfsP2L = [length (getPath [end] v end k) -1 | (k,v)<-cfsP2, Map.size v > 0]
    print $ "Day 12, Part 1: " ++ show (length path - 1)
    print $ "Day 12, Part 2: " ++ show (minimum cfsP2L)

getPath :: [Point] -> C -> Point -> Point -> [Point]
getPath ls cf end start
  | h == start = ls
  | otherwise = getPath ((cf ! h):ls) cf end start
  where h = head ls

parseMap :: Map.Map Point Char -> Int -> Int -> [Char] -> Map.Map Point Char
parseMap m x y [] = m
parseMap m x y ('\n':xs) = parseMap m 0 (y+1) xs
parseMap m x y (h:xs) = parseMap nm (x+1) y xs
  where
    nm = Map.insert (x,y) h m

aStar :: Point -> Point -> [Point] -> (M, D, D, C) -> C
aStar s e openSet (ws, gs, fs, cf)
  | null openSet = Map.empty
  | current == e = cf
  | otherwise = aStar s e nOS (ws, ngs, nfs, ncf)
  where
      current
        = fst
            $ minimumBy
                (compare `on` snd)
                [(v, Map.findWithDefault 99999999 v fs) | v <- openSet]
      (cx, cy) = current
      neighbors
        = [v |
             v <- [(cx - 1, cy), (cx + 1, cy), (cx, cy - 1), (cx, cy + 1)],
             Map.member v ws]
      (ngs, nfs, ncf, nnb)
        = aStarNeighbors e [] neighbors current (ws, gs, fs, cf)
      nOS = [v | v <- nnb, v `notElem` openSet] ++ [v | v<-openSet, v /= current]

distance :: Point -> Point -> Int
distance (ax,ay) (bx,by) = abs (ax - bx) + abs (ay - by)

aStarNeighbors :: Point -> [Point] -> [Point] -> Point -> (M, D, D, C) -> (D, D, C, [Point])
aStarNeighbors e pts [] current (ws, gs, fs, cf) = (gs, fs, cf, pts)
aStarNeighbors e pts (x:xs) current (ws, gs, fs, cf) = aStarNeighbors e np xs current (ws, ngs, nfs, ncf)
  where
    (cd, nd) = (getValue (ws ! current), getValue (ws ! x))
    tentativeGS = gs ! current + if nd - cd <= 1 then 1 else 99999999
    dist = distance x e
    (ncf, ngs, nfs, np) = if tentativeGS < Map.findWithDefault 99999999 x gs then
      (Map.insert x current cf, Map.insert x tentativeGS gs, Map.insert x (tentativeGS + dist) gs, x:pts)
    else
      (cf, gs, fs, pts)

getValue :: Char -> Int
getValue 'S' = ord 'a'
getValue 'E' = ord 'z'
getValue x = ord x

