module Day16 where

import Data.List.Split (splitOn)
import Data.List (sort, intersect)
import qualified Data.Set as Set
import Data.Char (ord, digitToInt)
import Debug.Trace (trace)
import GHC.Unicode (isLower)
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Function (on)
import GHC.OldList (tails)
import Data.Foldable (maximumBy)

type Valve = (String, Int, [String])
type Node = (Valve, Int)

type M = Map.Map String Valve
type D = Map.Map String (Map.Map String Int)

key :: (a,b,c) -> a
key (a,b,c) = a

rate :: M -> String -> Int
rate m k = let (kk, kr, kn) = m ! k in kr

run :: IO ()
run = do
    ls <- Map.fromList . map parseLine . lines <$> readFile "src/day16.txt"
    let
      bfs' from to = bfs (ls ! from) (ls ! to) ls (Map.singleton from (ls ! from)) [ls ! from]
      paths = Map.fromList [(from, Map.fromList [let p = bfs' from to in (to, p) |
            to <- Map.keys ls,
            rate ls to /= 0,
            from /= to]) |
        from <- Map.keys ls,
        from == "AA" || rate ls from /= 0]
      ds = bfsPath paths ls [(30, 0, ["AA"])] []
    print $ "Day 16, Part 1: " ++ show (maximum $ map fst ds)
    let possibles = bfsPath paths ls [(26, 0, ["AA"])] []
        pairs = [(xi+yi,(xi,xp),(yi,yp)) | ((xi,xp):ys) <- tails possibles, (yi, yp) <- ys, null (xp `intersect` yp)]
        mv = maximumBy (compare `on` key) pairs
    print $ "Day 16, Part 2: " ++ show (key mv)

bfsPath :: D -> M -> [(Int,Int,[String])] -> [(Int, [String])]  -> [(Int, [String])]
bfsPath dist m [] pp = pp
bfsPath dist m (x@(t, p, []):xs) pp = error "Empty path"
bfsPath dist m ((t, p, path@(cur:cs)):tl) pp
  | null new = bfsPath dist m tl ((p,init path):pp)
  | otherwise = bfsPath dist m (new ++ tl) pp
    where
      new = [(t-l-1, p + rate m k * (t-l-1), k:path) | (k,l)<-Map.toList (dist ! cur), l <= t - 2 && k `notElem` path]

bfs :: Valve -> Valve -> M -> M -> [Valve] -> Int
bfs s e graph prev [] = error $ "Could not find " ++ show e
bfs s e graph prev (x@(xk, xr, xn):xs)
  | x == e = getDist (key s) (key e) prev graph 0
  | otherwise = bfs s e graph np nq
    where
      getDist s curr p m d
        | s == curr = d
        | otherwise = getDist s (key new) p m (d+1)
        where
          new = p ! curr
      neighbors = [graph ! n | n<-xn, not $ Map.member n prev]
      np = updatePrev neighbors x prev
      nq = xs ++ neighbors

updatePrev :: [Valve] -> Valve -> M -> M
updatePrev [] v prev = prev
updatePrev (w:xs) v prev = updatePrev xs v np
    where
      np = Map.insert (key w) v prev

parseLine :: String -> (String, Valve)
parseLine l = (k, (k, rate, vvs))
  where
    (_:k:_:_:rt:_:_:_:_:xs) = words l
    rate = read (drop 5 (init rt)) :: Int
    vvs = splitOn "," (concat xs)