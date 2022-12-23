module Day23 where

import Debug.Trace (trace)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List
import Data.Maybe (fromJust)
import Data.List.Split (splitOn)
import Data.Function

type Point = (Int, Int)
type Look = Point -> Set.Set Point -> [Point]
type Move = Point -> Point

run :: IO ()
run = do
    ls <- parseElves <$> readFile "src/day23.txt"
    let lm = [(lookNorth, \(x,y) -> (x,y-1)), (lookSouth, \(x,y) -> (x,y+1)),
                (lookWest, \(x,y) -> (x-1,y)), (lookEast, \(x,y) -> (x+1,y))]
        pts = stepN 10 ls lm
        maxx = maximum $ map fst pts
        maxy = maximum $ map snd pts
        minx = minimum $ map fst pts
        miny = minimum $ map snd pts
    print $ "Day 23, Part 1: " ++ show ((1+maxy-miny) * (1+maxx-minx) - length pts)
    let v = stepWhile 1 ls lm
    print $ "Day 23, Part 2: " ++ show v

stepN :: Int -> [Point] -> [(Look, Move)] -> [Point]
stepN 0 p looks = p
stepN n p looks = stepN (n-1) (step p looks) (tail looks ++ [head looks])

stepWhile :: Int -> [Point] -> [(Look, Move)] -> Int
stepWhile n p looks = if Set.fromList p == Set.fromList np then 
            n
        else
            stepWhile (n+1) np (tail looks ++ [head looks])
    where
        np = step p looks

step :: [Point] -> [(Look, Move)] -> [Point]
step pts looks = --trace (toString pts)
        sh (fh pts looks Map.empty)
    where
        cache = Set.fromList pts
        fh [] lks m = m
        fh (p:xs) [(lookA, mvA),(lookB, mvB),(lookC, mvC),(lookD, mvD)] m
            | null $ lookAround p cache = fh xs looks (Map.insert p p m)
            | null $ lookA p cache = fh xs looks (Map.insert p (mvA p) m)
            | null $ lookB p cache = fh xs looks (Map.insert p (mvB p) m)
            | null $ lookC p cache = fh xs looks (Map.insert p (mvC p) m)
            | null $ lookD p cache = fh xs looks (Map.insert p (mvD p) m)
            | otherwise = fh xs looks (Map.insert p p m)
        fh _ _ _ = error "Invalid looks"
        sh m = [if Set.member v cfcts then k else v | (k,v)<-Map.toList m]
            where
                cfcts = Set.fromList $ map head $ filter (\x -> length x > 1) $ group . sort $ Map.elems m

toString :: [Point] -> String
toString pts =
    let maxx = maximum $ map fst pts
        maxy = maximum $ map snd pts
        minx = minimum $ map fst pts
        miny = minimum $ map snd pts
        getChar (x,y) = if (x,y) `elem` pts then "#" else "."
    in
        concat [if x == maxx+1 then getChar (x,y) ++ "\n" else getChar (x,y) | y<-[miny-1..maxy+1], x<-[minx-1..maxx+1]]

lookAround :: Point -> Set.Set Point -> [Point]
lookAround (x,y) s = filter (`Set.member` s) [
    (x,y-1), (x-1,y-1),(x+1,y-1), (x-1,y), (x+1,y), (x,y+1), (x-1,y+1),(x+1,y+1)
    ]

lookNorth :: Point -> Set.Set Point -> [Point]
lookNorth (x,y) s = filter (`Set.member` s) [(x,y-1), (x-1,y-1),(x+1,y-1)]

lookSouth :: Point -> Set.Set Point -> [Point]
lookSouth (x,y) s = filter (`Set.member` s) [(x,y+1), (x-1,y+1),(x+1,y+1)]

lookEast :: Point -> Set.Set Point -> [Point]
lookEast (x,y) s = filter (`Set.member` s) [(x+1,y-1), (x+1,y),(x+1,y+1)]

lookWest :: Point -> Set.Set Point -> [Point]
lookWest (x,y) s = filter (`Set.member` s) [(x-1,y-1), (x-1,y),(x-1,y+1)]

parseElves :: [Char] -> [Point]
parseElves c = go c (0,0) []
    where
        go [] (x,y) v = v
        go (ch:xs) (x,y) v
            | ch == '\n' = go xs (0, y+1) v
            | ch == '.' = go xs (x+1, y) v
            | ch == '#' = go xs (x+1, y) ((x,y):v)
            | otherwise = error $ "Unknown item: " ++ show ch