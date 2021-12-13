module Day05 (parseLines, part1, part2) where

import Data.Text (pack, splitOn, unpack)
import Control.Applicative ( Applicative(liftA2), Alternative (empty) )
import Text.Read (readMaybe)
import Data.Map (Map)
import qualified Data.Map as Map (delete, fromList, insertWith, keys, filter, lookup, update, (!), size, empty)

data Point = Point Int Int deriving (Show, Eq, Ord)

data Line = Line Point Point deriving (Show, Eq)

part1 :: [Line] -> Int
part1 = run . filter oblique

part2 :: [Line] -> Int
part2 = run

run :: [Line] -> Int
run = Map.size . Map.filter (>= 2) . foldl countit Map.empty

enumerateLine :: Line -> [Point]
enumerateLine l@(Line (Point x1 y1) (Point x2 y2)) =
  [Point x y | (x, y) <- (x2, y2) : takeWhile (/= (x2, y2)) (iterate (\(x, y) -> (x + dx, y + dy)) (x1, y1))]
  where
    (dx, dy) = gradient l

gradient :: Line -> (Int, Int)
gradient (Line (Point x1 y1) (Point x2 y2)) =
  (dx `div` g, dy `div` g)
  where
    dy = y2 - y1
    dx = x2 - x1
    g = gcd (abs dy) (abs dx)
    gcd a b = if b == 0 then a else gcd b (a `mod` b)

countit :: Map Point Int -> Line -> Map Point Int
countit m l =
  foldl f m (enumerateLine l)
    where
      f m p = Map.insertWith (+) p 1 m

oblique :: Line -> Bool
oblique (Line (Point x1 y1) (Point x2 y2)) =
  x1 == x2 || y1 == y2

parseLines :: [String] -> Maybe [Line]
parseLines = traverse points
  where
    points = parsePoint . words
    readPoint = mkPoint . ints
    ints = traverse (readMaybe . unpack) . splitOn (pack ",") . pack
    parsePoint [xy1, "->", xy2] = liftA2 Line (readPoint xy1) (readPoint xy2)
    parsePoint _ = Nothing
    mkPoint (Just [x, y]) = Just $ Point x y
    mkPoint _ = Nothing