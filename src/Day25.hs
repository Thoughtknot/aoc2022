{-# LANGUAGE BangPatterns #-}
module Day25 where

import Debug.Trace (trace)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List
import Data.Maybe (fromJust)
import Data.List.Split (splitOn)
import Data.Function

run :: IO ()
run = do
    snafus <- lines <$> readFile "src/day25.txt"
    let snafusDec = map snafuToDecimal snafus
    print $ "Day 25, Part 1: " ++ show (decimalToSnafu $ sum snafusDec)

decimalToSnafu :: Int -> String
decimalToSnafu v = go v ""
    where
        go 0 s = s
        go v s = case v `mod` 5 of
            0 -> go (v `div` 5) ('0':s)
            1 -> go ((v-1) `div` 5) ('1':s)
            2 -> go ((v-2) `div` 5) ('2':s)
            3 -> go ((v+2) `div` 5) ('=':s)
            4 -> go ((v+1) `div` 5) ('-':s)
            _ -> error "Impossible"

snafuToDecimal :: String -> Int
snafuToDecimal cs = go (reverse cs) 1 0
    where
        go :: [Char] -> Int -> Int -> Int
        go [] m v = v
        go (x:xs) m v = go xs (m*5) (v+m*getVal x)

getSnafu :: Int -> Char
getSnafu c = case c of
    2 -> '2'
    1 -> '1'
    0 -> '0'
    -1 -> '-'
    -2 -> '='
    _ -> error "Impossible"

getVal :: Char -> Int
getVal c = case c of
    '2' -> 2
    '1' -> 1
    '0' -> 0
    '-' -> -1
    '=' -> -2
    _ -> error "Impossible"