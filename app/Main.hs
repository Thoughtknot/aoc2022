module Main where

import System.Environment (getArgs)
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9

main :: IO ()
main = do
    args <- getArgs
    let day = read (head args)
    case day of
        1 -> Day1.run
        2 -> Day2.run
        3 -> Day3.run
        4 -> Day4.run
        5 -> Day5.run
        6 -> Day6.run
        7 -> Day7.run
        8 -> Day8.run
        9 -> Day9.run
        _ -> error $ "Unknown day: " ++ show day
