module Day10 where

import Data.List.Split (splitOn)
import Data.List (sort)
import qualified Data.Set as Set
import Data.Char (ord, digitToInt)
import Debug.Trace (trace)
import GHC.Unicode (isLower)
import Data.Map ((!))
import qualified Data.Map as Map

data Instruction = ADDX Int | NOOP

run :: IO ()
run = do
    ls <- map parseInstruction . lines <$> readFile "src/day10.txt"
    let result = executeInstructions Map.empty ls 1 0
        strengths = [ v * result ! v | v <- [20,60,100,140,180,220]]
        image = getImage result
    print $ "Day 10, Part 1: " ++ show (sum strengths)
    print "Day 10, Part 2: "
    putStr image
    print "Done"

getImage :: Map.Map Int Int -> String
getImage m = concat [getCRTChar x (x+1+y*40) |y<-[0..5], x<-[0..39]]
    where
        getCRTChar x c
          | abs ((m ! c)-x) <= 1 = if x == 39 then "#\n" else "#"
          | otherwise = if x == 39 then ".\n" else "."

executeInstructions :: Map.Map Int Int -> [Instruction] -> Int -> Int -> Map.Map Int Int
executeInstructions m [] x c = m
executeInstructions m (h:t) x c = case h of
  ADDX n -> executeInstructions (Map.insert (c+2) x (Map.insert (c+1) x m)) t (x+n) (c+2)
  NOOP -> executeInstructions (Map.insert (c+1) x m) t x (c+1)

parseInstruction :: String -> Instruction
parseInstruction ('n':'o':'o':'p':t) = NOOP
parseInstruction ('a':'d':'d':'x':' ':t) = ADDX (read t)
parseInstruction ls = error $ "Unknown string " ++ ls