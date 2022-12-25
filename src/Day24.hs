{-# LANGUAGE BangPatterns #-}
module Day24 where

import Debug.Trace (trace)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List
import Data.Maybe (fromJust)
import Data.List.Split (splitOn)
import Data.Function

type Blizzard = [(Point, Direction)]
type Point = (Int, Int)
data Direction = N | S | W | E deriving Show

run :: IO ()
run = do
    (wl, blz) <- parseMap <$> readFile "src/day24.txt"
    let minx = minimum . map fst $ Set.toList wl
        maxx = maximum . map fst $ Set.toList wl
        miny = minimum . map snd $ Set.toList wl
        maxy = maximum . map snd $ Set.toList wl
        start = head [(x,miny) | x<-[minx..maxx], not (Set.member (x,miny) wl)]
        end = head [(x,maxy) | x<-[minx..maxx], not (Set.member (x,maxy) wl)]
        times = bfs Set.empty (start, end) ((minx,maxx),(miny,maxy)) [(0,start,blz)]
    print $ "Day 24, Part 1: " ++ show (fst times)
    let timeBack = bfs Set.empty (end, start) ((minx,maxx),(miny,maxy)) [(fst times,end,snd times)]
        timeAgain = bfs Set.empty (start, end) ((minx,maxx),(miny,maxy)) [(fst timeBack,start,snd timeBack)]
    print $ "Day 24, Part 2: " ++ show (fst timeAgain)

bfs :: Set.Set (Int, Point) -> (Point,Point) -> (Point, Point) -> [(Int, Point, Blizzard)] -> (Int, Blizzard)
bfs !seen (!s,!e) !w [] = error "Could not find exit"
bfs !seen (!s,!e) w@((!minx,!maxx),(!miny,!maxy)) stck@((!t,p@(!x,!y),!blz):xs)
    | Set.member (t,p) seen = bfs ns (s,e) w xs
    | p == e = (t,blz)
    | otherwise =
            bfs ns (s,e) w (xs ++ new)
        where
            !ns = Set.insert (t,p) seen
            !nblz = moveBlizzard w blz
            !new = [(t+1,v,nblz) | v@(!vx,!vy)<-[(x,y), (x-1,y), (x+1,y), (x,y-1), (x,y+1)], Set.notMember (t+1,v) seen, v == e || v == s || (v `notElem` map fst nblz && vx > minx && vx < maxx && vy > miny && vy < maxy)]

moveBlizzard :: (Point,Point) -> [(Point, Direction)] -> [(Point, Direction)]
moveBlizzard ((!minx,!maxx),(!miny,!maxy)) !blz = go blz []
    where
        go [] !rs = rs
        go (h@(p@(!x,!y),!d):xs) !rs
            | npx > minx && npx < maxx && npy > miny && npy < maxy = go xs (((npx,npy),d):rs)
            | otherwise = case d of
                N -> go xs (((x,maxy-1),d):rs)
                S -> go xs (((x,miny+1),d):rs)
                W -> go xs (((maxx-1,y),d):rs)
                E -> go xs (((minx+1,y),d):rs)
            where
                (npx,npy) = case d of
                    N -> (x,y-1)
                    S -> (x,y+1)
                    W -> (x-1,y)
                    E -> (x+1,y)

parseMap :: [Char] -> (Set.Set Point, [(Point,Direction)])
parseMap ls = go ls (0,0) Set.empty []
    where
        go [] p s m = (s,m)
        go (c:xs) p@(x,y) s m
            | c == '\n' = go xs (0,y+1) s m
            | c == '#' = go xs (x+1,y) (Set.insert (x,y) s) m
            | c == '.' = go xs (x+1,y) s m
            | c == '>' = go xs (x+1,y) s (((x,y), E):m)
            | c == '<' = go xs (x+1,y) s (((x,y), W):m)
            | c == '^' = go xs (x+1,y) s (((x,y), N):m)
            | c == 'v' = go xs (x+1,y) s (((x,y), S):m)
            | otherwise = error ("Unknown char: " ++ [c])