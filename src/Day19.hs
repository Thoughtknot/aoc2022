{-# LANGUAGE BangPatterns #-}
module Day19 where

import Debug.Trace (trace)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type Blueprint = (Int, (Int, Int, (Int, Int), (Int, Int)))
data State = State {
  tm :: Int,
  ore :: Int,
  clay :: Int,
  obsidian :: Int,
  geode :: Int,
  oreRobots :: Int,
  clayRobots :: Int,
  obsidianRobots :: Int,
  geodeRobots :: Int
} deriving (Ord, Eq, Show)

run :: IO ()
run = do
    -- takes like 30 minutes to run... Will optimize later
    blueprints <- map parseBlueprint . lines <$> readFile "src/day19.txt"
    let initialState = State{tm=24, ore=0, clay=0, obsidian=0, geode=0, oreRobots=1, clayRobots=0, obsidianRobots=0, geodeRobots=0}
        paths = sum [bi * (bfs b Set.empty Map.empty [initialState] ! 0) | b@(bi,_) <- blueprints]
    print $ "Day 19, Part 1: " ++ show paths
    let initialStateP2 = State{tm=32, ore=0, clay=0, obsidian=0, geode=0, oreRobots=1, clayRobots=0, obsidianRobots=0, geodeRobots=0}
        pathsP2 = product [bfs b Set.empty Map.empty [initialStateP2] ! 0 | b <- take 3 blueprints]
    print $ "Day 19, Part 2: " ++ show pathsP2

bfs :: Blueprint -> Set.Set State -> Map.Map Int Int -> [State] -> Map.Map Int Int
bfs !bp !seen !maxCache [] = maxCache
bfs !bp !seen !maxCache stack@(state:xs)
  | Set.member state seen =
      bfs bp seen maxCache xs
  | null new = --trace (show stack) 
      bfs bp newSeen maxCache xs
  | otherwise = --trace (show stack) 
      bfs bp newSeen updatedMaxCache (new ++ xs)
  where
    new = [ps | !ps<-possibleStates bp state, not (Map.member (tm ps) maxCache) || (maxCache ! tm ps <= geode ps)]
    newSeen = Set.insert state seen
    updatedMaxCache = Map.union
      (Map.fromList [(tm v, geode v) | !v <- new, not (Map.member (tm v) maxCache) || (maxCache ! tm v < geode v)])
      maxCache

possibleStates :: Blueprint -> State -> [State]
possibleStates !bp state@State{tm=t}
  | t == 1 = [ns]
  | t == 0 = []
  | otherwise = [ns{
          ore=ore ns - oc, clay=clay ns - cc, obsidian=obsidian ns - obc,
          oreRobots=oreRobots ns + po, clayRobots=clayRobots ns + pc,
          obsidianRobots=obsidianRobots ns + pob, geodeRobots=geodeRobots ns + pg
        }
      | ((!po,!pc,!pob,!pg),(!oc,!cc,!obc,_))<-possiblePurchases bp state]
    where
      ns = generateOre state

type Ore = (Int, Int, Int, Int)
type Robots = (Int, Int, Int, Int)

possiblePurchases :: Blueprint -> State -> [(Robots, Ore)]
possiblePurchases (!k, (!o, !c, (!obo, !obc), (!go, !gob))) !state
  | not (null buildGeodeRobot) = buildGeodeRobot
  | otherwise = [((0,0,0,0),(0,0,0,0))] ++ buildOreRobot ++ buildClayRobot ++ buildObsidianRobot
  where
    maxOre = maximum [o, c, obo, go]
    buildOreRobot = [((1,0,0,0),(o,0,0,0)) | ore state >= o, oreRobots state < maxOre]
    buildClayRobot = [((0,1,0,0),(c,0,0,0)) | ore state >= c, clayRobots state < obc]
    buildObsidianRobot = [((0,0,1,0),(obo,obc,0,0)) | ore state >= obo, clay state >= obc, obsidianRobots state < gob]
    buildGeodeRobot = [((0,0,0,1),(go,0,gob,0))  | ore state >= go, obsidian state >= gob]

generateOre :: State -> State
generateOre !state = state{
  tm=tm state - 1,
  ore=ore state + oreRobots state,
  clay=clay state + clayRobots state,
  obsidian=obsidian state + obsidianRobots state,
  geode=geode state + geodeRobots state
}

parseBlueprint :: String -> Blueprint
parseBlueprint s = (read (init i), (read o, read c, (read obo, read obc), (read go, read gob)))
  where
    [_, i, _, _, _, _, o, _, _, _, _, _, c, _, _, _, _, _, obo, _, _, obc, _, _, _, _, _, go, _, _, gob, _] = words s