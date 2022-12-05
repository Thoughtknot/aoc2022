module Day5 where

import Data.List.Split (splitOn)
import Data.List (sort)
import qualified Data.Map as Map
import Data.Char (ord)
import Debug.Trace (trace)
import GHC.Unicode (isLower)
import qualified Prelude as Map
import Prelude
import Data.Map ((!))

run :: IO ()
run = do
    ls <- readFile "src/day5.txt"
    let [stack, ins] = splitOn "\n\n" ls
        parsedStack = parseStack (init $ lines stack) Map.empty
        instructions = map parseInstruction (lines ins)
        nm = executeInstructions instructions parsedStack
        word = concat [head $ nm ! k | k <- [1..maximum $ Map.keys nm]]
        nm2 = executeInstructionsPart2 instructions parsedStack
        word2 = concat [head $ nm2 ! k | k <- [1..maximum $ Map.keys nm2]]
    print $ "Day 5, Part 1: " ++ show word
    print $ "Day 5, Part 2: " ++ show word2
    print "Done"

executeInstructionsPart2 :: [(Int, Int, Int)] -> Map.Map Int [String] -> Map.Map Int [String]
executeInstructionsPart2 [] m = m
executeInstructionsPart2 ((mv,frm,to):xs) m = executeInstructionsPart2 xs nm
    where
        ov = m ! frm
        nov = drop mv ov
        nv = take mv ov ++ m ! to
        nm = Map.insert to nv (Map.insert frm nov m)

executeInstructions :: [(Int, Int, Int)] -> Map.Map Int [String] -> Map.Map Int [String]
executeInstructions [] m = m
executeInstructions ((mv,frm,to):xs) m = executeInstructions xs nm
    where
        ov = m ! frm
        nov = drop mv ov
        nv = reverse (take mv ov) ++ m ! to
        nm = Map.insert to nv (Map.insert frm nov m)

parseInstruction :: String -> (Int, Int, Int)
parseInstruction line = (read mv, read frm, read to)
    where [_,mv,_,frm,_,to] = splitOn " " line

parseStack :: [String] -> Map.Map Int [String] -> Map.Map Int [String]
parseStack [] m = m
parseStack (x:xs) m = parseStack xs nm
    where
        nm = parseStackRow x m 1

parseStackRow :: [Char] -> Map.Map Int [String] -> Int -> Map.Map Int [String]
parseStackRow [] m v = m
parseStackRow (' ':' ':' ':' ':xs) m v = parseStackRow xs m (v+1)
parseStackRow (' ':xs) m v = parseStackRow xs m v
parseStackRow ('[':xs) m v = parseStackRow xs m v
parseStackRow (']':xs) m v = parseStackRow xs m v
parseStackRow (x:xs) m v = parseStackRow xs nm (v+1)
    where
        vk = v --(v `div` 2) + 1
        nl = if Map.member vk m then m ! vk else []
        nm = Map.insert vk (nl ++ [[x]]) m
