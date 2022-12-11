module Day11 where

import Text.Show.Functions
import Data.List.Split (splitOn)
import Data.List (sort)
import qualified Data.Set as Set
import Data.Char (ord, digitToInt)
import Debug.Trace (trace)
import GHC.Unicode (isLower)
import Data.Map ((!))
import qualified Data.Map as Map

type Operation = Int -> Int
data Test = Test {
  divisible :: Int,
  t :: Int,
  f :: Int
} deriving Show
data Monkey = Monkey {
  mid :: Int,
  itemList :: [Int],
  opn :: Operation,
  tst :: Test
} deriving Show

run :: IO ()
run = do
    monkeys <- map parseMonkey . splitOn "\n\n" <$> readFile "src/day11.txt"
    let items = Map.fromList [(mid m, itemList m) | m <- monkeys]
        (newItems, inspections) = runRounds False monkeys 20 (items, Map.fromList [(mid m, 0) | m <- monkeys])
        [top1, top2] = take 2 $ reverse $ sort $ Map.elems inspections
        (newItems2, inspections2) = runRounds True monkeys 10000 (items, Map.fromList [(mid m, 0) | m <- monkeys])
        [top1Part2, top2Part2] = take 2 $ reverse $ sort $ Map.elems inspections2
    print $ "Day 11, Part 1: " ++ show (top1 * top2)
    print $ "Day 11, Part 2: " ++ show (top1Part2 * top2Part2)
    print "Done"

runRounds :: Bool -> [Monkey] -> Int -> (Map.Map Int [Int], Map.Map Int Int) -> (Map.Map Int [Int], Map.Map Int Int)
runRounds part2 mks 0 (mm,mc) = (mm, mc)
runRounds True mks i (mm,mc) = runRounds True mks (i-1) (runRoundP2 mks mks mm mc)
runRounds False mks i (mm,mc) = runRounds False mks (i-1) (runRound mks mm mc)

runRoundP2 :: [Monkey] -> [Monkey] -> Map.Map Int [Int] -> Map.Map Int Int -> (Map.Map Int [Int], Map.Map Int Int)
runRoundP2 mks [] its cnt = (its, cnt)
runRoundP2 mks (m:xs) its cnt = let (nits, nct) = runMonkeyPart2 mks (its ! mid m) m its cnt in runRoundP2 mks xs nits nct

runRound :: [Monkey] -> Map.Map Int [Int] -> Map.Map Int Int -> (Map.Map Int [Int], Map.Map Int Int)
runRound [] its cnt = (its, cnt)
runRound (m:xs) its cnt = let (nits, nct) = runMonkey (its ! mid m) m its cnt in runRound xs nits nct

runMonkeyPart2 :: [Monkey] -> [Int] -> Monkey -> Map.Map Int [Int] -> Map.Map Int Int -> (Map.Map Int [Int], Map.Map Int Int)
runMonkeyPart2 mks [] m its cnt = (Map.insert (mid m) [] its, cnt)
runMonkeyPart2 mks (x:xs) m its cnt = runMonkeyPart2 mks xs m nits ncnt
  where
    prodmod = product [divisible $ tst mk | mk <- mks]
    new = opn m x `mod` prodmod
    tstV = tst m
    nkey = if new `rem` divisible tstV == 0 then t tstV else f tstV
    ov = its ! nkey
    oc = cnt ! mid m
    nits = Map.insert nkey (ov ++ [new]) its
    ncnt = Map.insert (mid m) (oc + 1) cnt

runMonkey :: [Int] -> Monkey -> Map.Map Int [Int] -> Map.Map Int Int -> (Map.Map Int [Int], Map.Map Int Int)
runMonkey [] m its cnt = (Map.insert (mid m) [] its, cnt)
runMonkey (x:xs) m its cnt = runMonkey xs m nits ncnt
  where
    new = opn m x `div` 3
    tstV = tst m
    nkey = if new `rem` divisible tstV == 0 then t tstV else f tstV
    ov = its ! nkey
    oc = cnt ! mid m
    nits = Map.insert nkey (ov ++ [new]) its
    ncnt = Map.insert (mid m) (oc + 1) cnt

parseMonkey :: String -> Monkey
parseMonkey str = Monkey (read i :: Int) items (parseOperation operation) (Test test cTrue cFalse)
  where
    [mky,strt,op,tst,tr,fs] = lines str
    [_, i] = words (init mky)
    [_, itm] = splitOn ": " strt
    items = map (\x -> read x :: Int) $ splitOn ", " itm
    [_, operation] = splitOn " = " op
    test = read (last (words tst)) :: Int
    cTrue = read (last (words tr)) :: Int
    cFalse = read (last (words fs)) :: Int


parseOperation :: String -> Operation
parseOperation s
      | '+' `elem` s = let [fst, snd] = splitOn " + " s in
        \old ->
          let f = if fst == "old" then old else (read fst :: Int)
              s = if snd == "old" then old else (read snd :: Int) in
          f + s
      | '*' `elem` s = let [fst, snd] = splitOn " * " s in
        \old ->
          let f = if fst == "old" then old else (read fst :: Int)
              s = if snd == "old" then old else (read snd :: Int) in
          f * s
      | otherwise = error $ "Unknown operation: " ++ s
