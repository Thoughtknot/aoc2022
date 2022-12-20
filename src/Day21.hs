module Day21 where

import Debug.Trace (trace)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List
import Data.Maybe (fromJust)

run :: IO ()
run = do
    ls <- lines <$> readFile "src/day21.txt"
    print $ "Day 21, Part 1: " ++ show 1
    print $ "Day 21, Part 2: " ++ show 2