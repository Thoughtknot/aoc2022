module Day18 where

import Data.List.Split (splitOn)
import Data.List (sort)
import qualified Data.Set as Set
import Data.Char (ord, digitToInt)
import Debug.Trace (trace)
import GHC.Unicode (isLower)
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Function
import Data.Foldable

type Cube = (Int, Int, Int)

run :: IO ()
run = do
    cubes <- map parseCube . lines <$> readFile "src/day18.txt"
    let cubeSet = Set.fromList cubes
        surfaceArea = computeSurfaceArea 0 cubes cubeSet
        interiorPoints = findInterior cubeSet
        exteriorSurfaceArea = computeSurfaceArea 0 cubes (Set.union cubeSet interiorPoints)
    print $ "Day 18, Part 1: " ++ show surfaceArea
    print $ "Day 18, Part 2: " ++ show exteriorSurfaceArea

findInterior :: Set.Set Cube -> Set.Set Cube
findInterior s = Set.fromList ns
  where
    ns = calculateInteriorCached [(x,y,z) | x<-[minx..maxx],y<-[miny..maxy],z<-[minz..maxz], not (Set.member (x,y,z) s)] Set.empty []
    calculateInteriorCached [] g vs = vs
    calculateInteriorCached (x:xs) g vs 
      | Set.member x g = calculateInteriorCached xs g vs
      | intr = calculateInteriorCached xs g (x:vs)
      | otherwise = calculateInteriorCached xs (Set.union g cache) vs
      where 
        (intr, cache) = isInterior x s
    cl = Set.toList s
    maxx = maximum  (map (\(a,b,c) -> a) cl)
    maxy = maximum  (map (\(a,b,c) -> b) cl)
    maxz = maximum  (map (\(a,b,c) -> c) cl)
    minx = minimum  (map (\(a,b,c) -> a) cl)
    miny = minimum  (map (\(a,b,c) -> b) cl)
    minz = minimum  (map (\(a,b,c) -> c) cl)

isInterior :: Cube -> Set.Set Cube -> (Bool, Set.Set Cube)
isInterior (a,b,c) s = bfs s [(a,b,c)] (Set.singleton (a,b,c)) 0
  where 
    cl = Set.toList s
    maxx = maximum  (map (\(a,b,c) -> a) cl) + 1
    maxy = maximum  (map (\(a,b,c) -> b) cl) + 1
    maxz = maximum  (map (\(a,b,c) -> c) cl) + 1
    goal = (maxx,maxy,maxz)
    bfs g [] e h = (True, e)
    bfs g (x:xs) e h
      | h >= 10000 = (False, e)
      | x == goal = (False, e)
      | otherwise = bfs g (xs ++ nv) (Set.union e (Set.fromList nv)) (h+1)
      where
        nv = [v | v <- neighbors x, not (Set.member v g), not (Set.member v e)]

neighbors :: Cube -> [Cube]
neighbors (x,y,z) = [(x+dx,y+dy,z+dz) | (dx,dy,dz)<-[(1,0,0), (-1,0,0), (0,1,0), (0,-1,0), (0,0,1), (0,0,-1)]]

computeSurfaceArea :: Int -> [Cube] -> Set.Set Cube -> Int
computeSurfaceArea a xs s = foldl (\ a x -> a + surface x) a xs
  where
    surface (x,y,z) = length [(x+dx,y+dy,z+dz) | (dx,dy,dz)<-[(1,0,0), (-1,0,0), (0,1,0), (0,-1,0), (0,0,1), (0,0,-1)], not (Set.member (x+dx,y+dy,z+dz) s)]

parseCube :: String -> Cube
parseCube s = let [a,b,c] = splitOn "," s in (read a, read b, read c)
