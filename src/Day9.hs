module Day9 where

import Data.List.Split (splitOn)
import Data.List (sort)
import qualified Data.Set as Set
import Data.Char (ord, digitToInt)
import Debug.Trace (trace)
import GHC.Unicode (isLower)
import Data.Map ((!))
import qualified Data.Map as Map

type Point = (Int, Int)
data Instruction = R Int | L Int | D Int | U Int

run :: IO ()
run = do
    ls <- concatMap parseInstuction . lines <$> readFile "src/day9.txt"
    let (h,t,s) = executeInstructions (Set.singleton (0,0)) ls (0,0) (0,0)
        (m,ns) = executeInstructionsPart2 (Set.singleton (0,0)) ls (Map.fromList [(v,(0,0)) | v <- [0..9]]) 0
    print $ "Day 9, Part 1: " ++ show (Set.size s)
    print $ "Day 9, Part 2: " ++ show (Set.size ns)
    print "Done"

executeInstructionsPart2 :: Set.Set Point -> [Instruction] -> Map.Map Int Point -> Int -> (Map.Map Int Point, Set.Set Point)
executeInstructionsPart2 s [] m c = (m, s)
executeInstructionsPart2 s (xi:xs) m c
    | c == 0 = executeInstructionsPart2 s (xi:xs) (Map.insert c (nhx,nhy) m) (c+1)
    | c == maxc = executeInstructionsPart2 ns xs (Map.insert c (ntx,nty) m) 0
    | otherwise = executeInstructionsPart2 s (xi:xs) (Map.insert c (ntx,nty) m) (c+1)
    where
        maxc = maximum (Map.keys m)
        (x,y) = m ! c
        (nhx,nhy) = case xi of
            R n -> (x+1,y)
            L n -> (x-1,y)
            D n -> (x,y+1)
            U n -> (x,y-1)
        (ntx,nty) = moveTail (m ! (c-1)) (x,y)
        ns = Set.insert (ntx,nty) s

executeInstructions :: Set.Set Point -> [Instruction] -> Point -> Point -> (Point, Point, Set.Set Point)
executeInstructions s [] h t = (h, t, s)
executeInstructions s (x:xs) (hx,hy) (tx,ty) = executeInstructions ns xs (nhx,nhy) (ntx,nty)
    where
        (nhx,nhy) = case x of
            R n -> (hx+1,hy)
            L n -> (hx-1,hy)
            D n -> (hx,hy+1)
            U n -> (hx,hy-1)
        (ntx,nty) = moveTail (nhx,nhy) (tx,ty)
        ns = Set.insert (ntx,nty) s

moveTail :: Point -> Point -> Point
moveTail (hx,hy) (tx,ty) = if abs dx <= 1 && abs dy <= 1 then
                            (tx,ty)
                        else
                            (ntx,nty)
                        where
                            dx = hx - tx
                            dy = hy - ty
                            (ntx,nty) = case (dx, dy) of
                                (-2, 0) -> (tx-1,ty)
                                (2, 0) -> (tx+1,ty)
                                (0, -2) -> (tx,ty-1)
                                (0, 2) -> (tx,ty+1)

                                (-2, 1) -> (tx-1,ty+1)
                                (-2, -1) -> (tx-1,ty-1)

                                (2, 1) -> (tx+1,ty+1)
                                (2, -1) -> (tx+1,ty-1)

                                (1, -2) -> (tx+1,ty-1)
                                (-1, -2) -> (tx-1,ty-1)

                                (1, 2) -> (tx+1,ty+1)
                                (-1, 2) -> (tx-1,ty+1)

                                (-2,2) -> (tx-1,ty+1)
                                (2,2) -> (tx+1,ty+1)
                                (2,-2) -> (tx+1,ty-1)
                                (-2,-2) -> (tx-1,ty-1)
                                (v,w) -> error $ "Unsupported position " ++ show (v,w)

parseInstuction :: String -> [Instruction]
parseInstuction ('R':' ':v) = [R 1 | k<-[1..(read v)]]
parseInstuction ('L':' ':v) = [L 1 | k<-[1..(read v)]]
parseInstuction ('U':' ':v) = [U 1 | k<-[1..(read v)]]
parseInstuction ('D':' ':v) = [D 1 | k<-[1..(read v)]]
parseInstuction x = error $ "Unknown instruction: " ++ x