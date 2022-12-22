module Day22 where

import Debug.Trace (trace)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List
import Data.Maybe (fromJust)
import Data.List.Split (splitOn)
import Data.Function

type Point = (Int, Int)
type Direction = (Int, Int)

type EdgeMap = Map.Map (Point, Direction) Point
data Instruction = F | L | R deriving Show

run :: IO ()
run = do
    ls <- readFile "src/day22.txt"
    let [h, t] = splitOn "\n\n" ls
        mp = parseMap h Map.empty (1,1)
        ins = parseInstructions t
        start = minimumBy (compare `on` fst) [(x,y) | ((x,y),v) <- Map.toList mp, y == 1, v == '.']
        end@((ex,ey),ed) = followInstructions ins start (1,0) mp
        edv = case ed of
            (1,0) -> 0
            (0,1) -> 1
            (-1,0) -> 2
            (0,-1) -> 3
            _ -> error "Invalid direction"
    print $ "Day 22, Part 1: " ++ show (1000*ey + 4*ex + edv)
    let 
        end2@((ex2,ey2),ed2) = followInstructionsPart2 ins start (1,0) mp (buildEdges mp)
        edv2 = case ed2 of
            (1,0) -> 0
            (0,1) -> 1
            (-1,0) -> 2
            (0,-1) -> 3
            _ -> error "Invalid direction"
    print $ "Day 22, Part 2: " ++ show (1000*ey2 + 4*ex2 + edv2)

buildEdgesTest :: Map.Map Point Char -> ((Point, Direction) -> (Point, Direction))
buildEdgesTest m = go m 
    where
        left = (-1,0)
        right = (1,0)
        up = (0,-1)
        down = (0,1)
        a = [(x,1) | x<-[9..12]]
        b = [(9,y) | y<-[1..4]]
        c = [(x,5) | x<-[5..8]]
        d = [(x,5) | x<-[1..4]]
        e = [(1,y) | y<-[5..8]]
        f = [(x,8) | x<-[1..4]]
        g = [(x,8) | x<-[5..8]]
        h = [(9,y) | y<-[9..12]]
        i = [(x,12) | x<-[9..12]]
        j = [(x,12) | x<-[13..16]]
        k = [(16,y) | y<-[9..12]]
        l = [(x,9) | x<-[13..16]]
        m = [(12,y) | y<-[5..8]]
        n = [(12,y) | y<-[1..4]]
        bc = Map.fromList $ zip b c
        cb = Map.fromList $ zip c b
        lm = Map.fromList $ zip (reverse l) m
        ml = Map.fromList $ zip m (reverse l)
        gh = Map.fromList $ zip (reverse g) h
        hg = Map.fromList $ zip h (reverse g)
        kn = Map.fromList $ zip (reverse k) n
        nk = Map.fromList $ zip n (reverse k)
        ifm = Map.fromList $ zip i (reverse f)
        fim = Map.fromList $ zip (reverse f) i
        je = Map.fromList $ zip j (reverse e)
        ej = Map.fromList $ zip (reverse e) j
        ad = Map.fromList $ zip a (reverse d)
        da = Map.fromList $ zip (reverse d) a
        goR m p
            | Map.member p kn = (kn ! p, left)
            | Map.member p nk = (nk ! p, left)
            | Map.member p ml = (ml ! p, down)
            | otherwise = error ("Point not in any right side: " ++ show (kn, p))
        goL m p
            | Map.member p ej = (ej ! p, up)
            | Map.member p bc = (bc ! p, down)
            | Map.member p hg = (hg ! p, up)
            | otherwise = error ("Point not in any left side: " ++ show p)
        goU m p
            | Map.member p ad = (ad ! p, down)
            | Map.member p da = (da ! p, down)
            | Map.member p cb = (cb ! p, right)
            | Map.member p lm = (lm ! p, left)
            | otherwise = error ("Point not in any top side: " ++ show p)
        goD m p
            | Map.member p fim = (fim ! p, up)
            | Map.member p ifm = (ifm ! p, up)
            | Map.member p je = (je ! p, right)
            | Map.member p gh = (gh ! p, right)
            | otherwise = error ("Point not in any down side: " ++ show p)
        go m ((x,y),(dx,dy)) =
            case (dx, dy) of
                (1,0) -> goR m (x,y)
                (-1,0) -> goL m (x,y)
                (0,1) -> goD m (x,y)
                (0,-1) -> goU m (x,y)
                _ -> error ("Invalid direction " ++ show (dx,dy))

buildEdges :: Map.Map Point Char -> ((Point, Direction) -> (Point, Direction))
buildEdges m = go m 
    where
        left = (-1,0)
        right = (1,0)
        up = (0,-1)
        down = (0,1)
        a = [(51,y) | y<-[1..50]]
        b = [(51,y) | y<-[51..100]]
        c = [(x,101) | x<-[1..50]]
        d = [(1,y) | y<-[101..150]]
        e = [(1,y) | y<-[151..200]]
        f = [(x,200) | x<-[1..50]]
        g = [(50,y) | y<-[151..200]]
        h = [(x,150) | x<-[51..100]]
        i = [(100,y) | y<-[101..150]]
        j = [(100,y) | y<-[51..100]]
        k = [(x,50) | x<-[101..150]]
        l = [(150,y) | y<-[1..50]]
        m = [(x,1) | x<-[101..150]]
        n = [(x,1) | x<-[51..100]]
        li = Map.fromList $ zip (reverse l) i
        il = Map.fromList $ zip i (reverse l)
        jk = Map.fromList $ zip j k
        kj = Map.fromList $ zip k j
        gh = Map.fromList $ zip g h
        hg = Map.fromList $ zip h g
        ad = Map.fromList $ zip (reverse a) d
        da = Map.fromList $ zip d (reverse a)
        bc = Map.fromList $ zip b c
        cb = Map.fromList $ zip c b
        en = Map.fromList $ zip e n
        ne = Map.fromList $ zip n e
        fm = Map.fromList $ zip f m
        mf = Map.fromList $ zip m f
        goR m p
            | Map.member p li = (li ! p, left)
            | Map.member p il = (il ! p, left)
            | Map.member p jk = (jk ! p, up)
            | Map.member p gh = (gh ! p, up)
            | otherwise = error ("Point not in any right side: " ++ show p)
        goL m p
            | Map.member p ad = (ad ! p, right)
            | Map.member p da = (da ! p, right)
            | Map.member p bc = (bc ! p, down)
            | Map.member p en = (en ! p, down)
            | otherwise = error ("Point not in any left side: " ++ show p)
        goU m p
            | Map.member p cb = (cb ! p, right)
            | Map.member p ne = (ne ! p, right)
            | Map.member p mf = (mf ! p, up)
            | otherwise = error ("Point not in any top side: " ++ show p)
        goD m p
            | Map.member p fm = (fm ! p, down)
            | Map.member p hg = (hg ! p, left)
            | Map.member p kj = (kj ! p, left)
            | otherwise = error ("Point not in any down side: " ++ show p)
        go m ((x,y),(dx,dy)) =
            case (dx, dy) of
                (1,0) -> goR m (x,y)
                (-1,0) -> goL m (x,y)
                (0,1) -> goD m (x,y)
                (0,-1) -> goU m (x,y)
                _ -> error ("Invalid direction " ++ show (dx,dy))

followInstructionsPart2 :: [Instruction] -> Point -> Direction -> Map.Map Point Char -> ((Point, Direction) -> (Point, Direction)) -> (Point, Direction)
followInstructionsPart2 [] p d m em = (p,d)
followInstructionsPart2 (L:xs) (x,y) (dx, dy) m em = followInstructionsPart2 xs (x,y) (dy, -dx) m em
followInstructionsPart2 (R:xs) (x,y) (dx, dy) m em = followInstructionsPart2 xs (x,y) (-dy, dx) m em
followInstructionsPart2 (F:xs) (x,y) (dx, dy) m em = case Map.lookup np m of
    Nothing -> if m ! wp == '#' then 
            --trace (show ((x,y), (dx,dy), wp, wd))
            followInstructionsPart2 xs (x,y) (dx,dy) m em
        else  
            --trace (show ((x,y), (dx,dy), wp, wd))
            followInstructionsPart2 xs wp wd m em
    Just '.' -> 
            --trace (show ((x,y), (dx,dy)))
            followInstructionsPart2 xs np (dx,dy) m em
    Just '#' -> 
            --trace (show ((x,y), (dx,dy)))
            followInstructionsPart2 xs (x,y) (dx,dy) m em
    _ -> error $ "Invalid map " ++ show (m, np)
    where
        np = (x + dx, y + dy)
        (wp, wd) = em ((x,y), (dx, dy))

followInstructions :: [Instruction] -> Point -> Direction -> Map.Map Point Char -> (Point, Direction)
followInstructions [] p d m = (p,d)
followInstructions (L:xs) (x,y) (dx, dy) m = followInstructions xs (x,y) (dy, -dx) m
followInstructions (R:xs) (x,y) (dx, dy) m = followInstructions xs (x,y) (-dy, dx) m
followInstructions (F:xs) (x,y) (dx, dy) m = case Map.lookup np m of
    Nothing -> if m ! wp == '#' then 
            followInstructions xs (x,y) (dx,dy) m 
        else  
            followInstructions xs wp (dx,dy) m
    Just '.' -> followInstructions xs np (dx,dy) m
    Just '#' -> followInstructions xs (x,y) (dx,dy) m
    _ -> error $ "Invalid map " ++ show (m, np)
    where
        np = (x + dx, y + dy)
        wp = case (dx, dy) of
            (1,0) -> minimumBy (compare `on` fst) [ (vx,vy) | (vx,vy) <- Map.keys m, vy == y]
            (-1,0) -> maximumBy (compare `on` fst) [ (vx,vy) | (vx,vy) <- Map.keys m, vy == y]
            (0,1) -> minimumBy (compare `on` snd) [ (vx,vy) | (vx,vy) <- Map.keys m, vx == x]
            (0,-1) -> maximumBy (compare `on` snd) [ (vx,vy) | (vx,vy) <- Map.keys m, vx == x]
            _ -> error $ "Invalid direction" ++ show (dx,dy)

parseMap :: String -> Map.Map Point Char -> Point -> Map.Map Point Char
parseMap [] m p = m
parseMap (x:xs) m p@(px, py) = case x of
    '\n' -> parseMap xs m (1, py+1)
    '.' -> parseMap xs (Map.insert p '.' m) (px+1,py)
    '#' -> parseMap xs (Map.insert p '#' m) (px+1,py)
    ' ' -> parseMap xs m (px+1,py)
    _ -> error $ "Unknown char " ++ [x]

parseInstructions :: String -> [Instruction]
parseInstructions s = go s [] []
    where
        go :: String -> String -> [Instruction] -> [Instruction]
        go [] [] i = reverse i
        go [] b i = go [] [] ([F | v<-[1..read b :: Int]] ++ i)
        go (x:xs) b i
            | x == 'R' = go xs [] (R:ni)
            | x == 'L' = go xs [] (L:ni)
            | otherwise = go xs (b ++ [x]) i
            where
                ni = if null b then i else [F | v<-[1..read b :: Int]] ++ i