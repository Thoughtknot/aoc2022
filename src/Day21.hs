module Day21 where

import Debug.Trace (trace)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List
import Data.Maybe (fromJust)
import Data.List.Split (splitOn)

data Instruction = Mul String String String
  | Add String String String
  | Sub String String String
  | Div String String String
  | Eq String String String
  | Atom String Int

run :: IO ()
run = do
    ls <- map parseInstruction . lines <$> readFile "src/day21.txt"
    let mapped = mapMonkeys (Map.fromList ls) Map.empty
    print $ "Day 21, Part 1: " ++ show (mapped ! "root")
    let nm = Map.fromList [if k == "root" then let Add i a b = v in (k, Eq i a b) else (k,v) | (k,v)<-ls]
        hm = iterateMonkey2 nm (0,10000000000000) 1000000000000
    print $ "Day 21, Part 2: " ++ show (fst hm)

iterateMonkey2 :: Map.Map String Instruction -> (Int,Int) -> Int -> (Int, Int)
iterateMonkey2 m (s,e) i 
    | any (\(x,y) -> y == 0) n = head [(k, v) | (k,v)<-n, v == 0]
    | otherwise = let (ns,ne) = findPivot n in iterateMonkey2 m (ns,ne) (i `div` 10)
    where 
      n = [(v,mapMonkeys (Map.insert "humn" (Atom "humn" v) m) Map.empty ! "root") | v<-[s,s+i..e]]
      findPivot ((a,x):(b,y):ls) = if x > 0 && y < 0 then (a,b) else findPivot ((b,y):ls)
      findPivot _ = error "Could not find pivot"

iterateMonkey :: Map.Map String Instruction -> Int -> Int -> Int -> Int
iterateMonkey m guess lastGuess lastVal
  | nm == 0 = guess
  | guess > lastGuess && nm > lastVal = iterateMonkey m (lastGuess + (guess - lastGuess) `div` 2) guess nm
  | guess > lastGuess && nm < lastVal = iterateMonkey m (2*guess) guess nm
  | guess < lastGuess && nm > lastVal = iterateMonkey m (lastGuess - (lastGuess - guess) `div` 2) guess nm
  | guess < lastGuess && nm < lastVal = iterateMonkey m (guess `div` 2) guess nm
  | otherwise = iterateMonkey m (guess+1) guess nm
  where
    nm = mapMonkeys (Map.insert "humn" (Atom "humn" guess) m) Map.empty ! "root"

mapMonkeys :: Map.Map String Instruction -> Map.Map String Int -> Map.Map String Int
mapMonkeys ins res
  | Map.member "root" res = res
  | otherwise = mapMonkeys ins (go (Map.toList ins) res)
    where
      go :: [(String, Instruction)] -> Map.Map String Int -> Map.Map String Int
      go [] v = v
      go ((k,x):xs) v = case x of
        Eq s a b -> case (Map.lookup a v, Map.lookup b v) of
          (Just aa, Just bb) -> go xs (Map.insert s (aa - bb) v)
          _ -> go xs v
        Mul s a b -> case (Map.lookup a v, Map.lookup b v) of
          (Just aa, Just bb) -> go xs (Map.insert s (aa * bb) v)
          _ -> go xs v
        Add s a b -> case (Map.lookup a v, Map.lookup b v) of
          (Just aa, Just bb) -> go xs (Map.insert s (aa + bb) v)
          _ -> go xs v
        Sub s a b -> case (Map.lookup a v, Map.lookup b v) of
          (Just aa, Just bb) -> go xs (Map.insert s (aa - bb) v)
          _ -> go xs v
        Div s a b -> case (Map.lookup a v, Map.lookup b v) of
          (Just aa, Just bb) -> go xs (Map.insert s (aa `div` bb) v)
          _ -> go xs v
        Atom s n -> go xs (Map.insert s n v)

parseInstruction :: String -> (String, Instruction)
parseInstruction ln
  | length w == 2 = let [a,b] = w in (init a, Atom (init a) (read b :: Int))
  | otherwise = let [a,b,c,d] = w in case (init a, c) of
    (_,"*") -> (init a, Mul (init a) b d)
    (_,"-") -> (init a, Sub (init a) b d)
    (_,"+") -> (init a, Add (init a) b d)
    (_,"/") -> (init a, Div (init a) b d)
    _ -> error $ "Unknown value: " ++ show w
  where
    w = words ln