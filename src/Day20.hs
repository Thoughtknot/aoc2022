{-# LANGUAGE BangPatterns #-}
module Day20 where

import Debug.Trace (trace)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List
import Data.Maybe (fromJust)

--Takes like ten minutes. Can't be bothered to fix.
run :: IO ()
run = do
    numbers <- map (\x -> read x :: Int) . lines <$> readFile "src/day20.txt"
    let idNumbers = zip [0..] numbers
        rearrange = map snd $ mixTimes idNumbers 1
        indexOfZero = fromJust $ elemIndex 0 rearrange
        sumOfThree = rearrange !! ((indexOfZero+1000) `mod` length rearrange)
            + rearrange !! ((indexOfZero+2000) `mod` length rearrange) 
            + rearrange !! ((indexOfZero+3000) `mod` length rearrange)
    let adjustedNumbers = map (\(x,y) -> (x,y*811589153)) idNumbers
        rearrangePart2 = map snd $ mixTimes adjustedNumbers 10
        indexOfZeroPart2 = fromJust $ elemIndex 0 rearrangePart2
        sumOfThreePart2 = rearrangePart2 !! ((indexOfZeroPart2+1000) `mod` length rearrangePart2)
            + rearrangePart2 !! ((indexOfZeroPart2+2000) `mod` length rearrangePart2) 
            + rearrangePart2 !! ((indexOfZeroPart2+3000) `mod` length rearrangePart2)
    print $ "Day 20, Part 1: " ++ show sumOfThree
    print $ "Day 20, Part 2: " ++ show sumOfThreePart2

mixTimes :: [(Int,Int)] -> Int -> [(Int,Int)]
mixTimes l 0 = l
mixTimes l v = mixTimes (mix l 0 (length l)) (v-1) 

mix :: [(Int,Int)] -> Int -> Int -> [(Int,Int)]
mix l i sz
  | i == sz = l
  | v > 0 = mix (moveRight l (k,v) ia) (i+1) sz
  | otherwise = mix (moveLeft l (k,v) ia) (i+1) sz
  where
    (k, v, ia) = head [(k, vv, fromJust $ elemIndex (k,vv) l) | (k,vv) <- l, k == i]

moveRight :: [(Int,Int)] -> (Int, Int) -> Int -> [(Int,Int)]
moveRight l it@(k,v) i = do
  let idx = (i+v) `mod` (length l - 1)
      nv = [p | p<-l, p /= it]
      (h, t) = splitAt idx nv
  h ++ it:t

moveLeft :: [(Int,Int)] -> (Int, Int) -> Int -> [(Int,Int)]
moveLeft l it@(k,v) i = do
  let idx = (i - abs v) `mod` (length l - 1)
      nv = [p | p<-l, p /= it]
      (h, t) = splitAt idx nv
  if idx == 0 then
    nv ++ [it]
  else
    h ++ it:t