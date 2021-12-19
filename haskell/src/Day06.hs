module Day06 (parseFish, part1, part2) where

import Data.Map (Map)
import qualified Data.Map as Map (empty, insertWith, mapKeys, updateLookupWithKey)
import Data.Text (pack, splitOn, unpack)
import Text.Read (readMaybe)

newtype Fish = Fish Int deriving (Eq, Show, Ord)

part1 :: [Fish] -> Integer
part1 = run 80

part2 :: [Fish] -> Integer
part2 = run 256

run :: Int -> [Fish] -> Integer
run n = sum . simulateDays n . zeroDay

simulateDays :: Int -> Map Fish Integer -> Map Fish Integer
simulateDays 0 m = m
simulateDays n m = simulateDays (n - 1) $ step m

step :: Map Fish Integer -> Map Fish Integer
step = reinsert . findBirths . simulateDay
  where
    reinsert (Just births, m) = Map.insertWith (+) (Fish 8) births $ Map.insertWith (+) (Fish 6) births m
    reinsert (Nothing, m) = m

simulateDay :: Map Fish Integer -> Map Fish Integer
simulateDay = Map.mapKeys (\(Fish d) -> Fish $ d - 1)

findBirths :: Map Fish a -> (Maybe a, Map Fish a)
findBirths = Map.updateLookupWithKey (\_ _ -> Nothing) (Fish $ -1)

zeroDay :: [Fish] -> Map Fish Integer
zeroDay = foldr (\f m -> Map.insertWith (+) f 1 m) Map.empty

parseFish :: String -> Maybe [Fish]
parseFish = traverse (fmap Fish . readMaybe . unpack) . splitOn (pack ",") . pack