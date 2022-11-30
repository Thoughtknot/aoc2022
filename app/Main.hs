module Main where

import System.Environment (getArgs)
import qualified Day1

main :: IO ()
main = do
    args <- getArgs
    let day = read (head args)
    case day of
        1 -> Day1.run
        _ -> error $ "Unknown day: " ++ show day
