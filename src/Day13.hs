module Day13 where

import Data.List.Split (splitOn)
import Data.List (sort)
import qualified Data.Set as Set
import Data.Char (ord, digitToInt)
import Debug.Trace (trace)
import GHC.Unicode (isLower)
import Data.Map ((!))
import qualified Data.Map as Map
import Text.Parsec (ParseError, oneOf, many1, digit, satisfy, char, anyChar, sepBy, parse, sepBy1, noneOf, letter, spaces, try, space)
import Text.Parsec.String (Parser)
import Control.Applicative ((<|>))
import Data.Bifunctor
import Data.Either (fromRight)
import Data.Sequence (sortBy)

data Packet = List [Packet] | Value Int
    deriving (Eq, Show)

instance Ord Packet where
  compare a b = case comparePackets (a, b) of
      RIGHT -> GT
      WRONG -> LT
      CONT -> EQ

data Result = RIGHT | WRONG | CONT deriving (Ord, Eq, Show)


run :: IO ()
run = do
    ls <- map (\x -> let [a,b] = lines x in (a,b)) . splitOn "\n\n" <$> readFile "src/day13.txt"
    let l = map (toRights . bimap (parse parsePacket "a") (parse parsePacket "b")) ls
        comparedPackets = zip [1..] $ map comparePackets l
        allPackets = concatMap (\(a,b) -> [a,b]) l
        dividers = [List [List [Value 2]], List [List [Value 6]]]
        sortedPackets = zip [1..] $ reverse (sort (allPackets ++ dividers))
        dividerIndexes = [k |(k,v)<- sortedPackets, v `elem` dividers]
    print $ "Day 13, Part 1: " ++ show (sum [i | (i,r) <- comparedPackets, r == RIGHT])
    print $ "Day 13, Part 2: " ++ show (head dividerIndexes * last dividerIndexes)

toRights :: (Either ParseError Packet, Either ParseError Packet) -> (Packet, Packet)
toRights (Right a, Right b) = (a,b)
toRights _ = error "Can't parse!"

comparePackets :: (Packet, Packet) -> Result
comparePackets (a,b) = res
  where
    res = compareSinglePackets a b

compareSinglePackets :: Packet -> Packet -> Result
compareSinglePackets (Value a) (Value b)
  | a < b = RIGHT
  | a > b = WRONG
  | otherwise = CONT
compareSinglePackets (List a) (List b) = compareLists a b
compareSinglePackets (Value a) (List b) = compareLists [Value a] b
compareSinglePackets (List a) (Value b) = compareLists a [Value b]

compareLists :: [Packet] -> [Packet] -> Result
compareLists [] [] = CONT
compareLists [] ls = RIGHT
compareLists ls [] = WRONG
compareLists (x:xs) (y:ys) = case compareSinglePackets x y of
  RIGHT -> RIGHT
  WRONG -> WRONG
  CONT -> compareLists xs ys

parsePacket :: Parser Packet
parsePacket = parseValue <|> parseList

parseValue = do
    d <- many1 digit
    return $ Value $ read d

parseList :: Parser Packet
parseList =  do
    char '['
    r <- sepBy parsePacket $ char ','
    char ']'
    return $ List r