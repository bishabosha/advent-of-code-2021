module Day04 (part1, part2, parseBingoGame) where

import Data.Map (Map)
import qualified Data.Map as Map (delete, fromList, insert, keys, lookup, update, (!))
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set (empty, insert, member)
import Data.Text (pack, splitOn, unpack)

newtype Board = Board {getBoard :: [[Int]]}
  deriving (Eq, Show)

data BingoGame = BingoGame {numbers :: [Int], boards :: [Board]}
  deriving (Show)

data ActiveGame = ActiveGame {toDraw :: [Int], games :: [PlayGame]}
  deriving (Show)

type N = Int

type Row = Int

type Col = Int

data PlayGame = PlayGame
  { indexed :: Map N (Row, Col),
    scores :: Bool,
    seen :: Set N,
    rows :: Map Row Int,
    cols :: Map Col Int
  }
  deriving (Show)

initBoard :: Board -> PlayGame
initBoard (Board b) =
  PlayGame
    { indexed = Map.fromList $ concatMap (\(r, xs) -> fmap (\(c, n) -> (n, (r, c))) (zip [0 ..] xs)) (zip [0 ..] b),
      scores = False,
      seen = Set.empty,
      rows = Map.fromList $ zip [0 ..] $ map (const 0) [0 .. 4],
      cols = Map.fromList $ zip [0 ..] $ map (const 0) [0 .. 4]
    }

-- if !seen n then indexed(n) map \(r, c) -> seen += n; rows ! r += 1; cols c += 1
updateGame :: N -> PlayGame -> PlayGame
updateGame n p@(PlayGame indexed scores seen rows cols) =
  if Set.member n seen
    then p
    else
      maybe
        p
        ( \(r, c) ->
            PlayGame
              (Map.delete n indexed)
              (Map.lookup r rows == Just 4 || Map.lookup c cols == Just 4)
              (Set.insert n seen)
              (Map.update addOne r rows)
              (Map.update addOne c cols)
        )
        (Map.lookup n indexed)
  where
    addOne = Just . (+ 1)

runGame :: ActiveGame -> Maybe Int
runGame (ActiveGame (n : ns) gs) =
  let gs' = map (updateGame n) gs
   in case filter scores gs' of
        [g] -> Just $ n * sum (Map.keys $ indexed g)
        _ : _ -> Nothing -- more than one game has a score?
        [] -> runGame (ActiveGame ns gs')
runGame (ActiveGame [] gs) = Nothing -- no more numbers to draw

runGame' :: ActiveGame -> Maybe Int
runGame' (ActiveGame (n : ns) [g]) =
  let g' = updateGame n g
   in if scores g'
        then Just $ n * sum (Map.keys $ indexed g')
        else runGame' (ActiveGame ns [g'])
runGame' (ActiveGame (n : ns) gs) =
  let gs' = map (updateGame n) gs
   in runGame' (ActiveGame ns (filter (not . scores) gs'))
runGame' (ActiveGame [] gs) = Nothing -- no more numbers to draw

initialGame :: BingoGame -> ActiveGame
initialGame (BingoGame numbers boards) = ActiveGame numbers $ map initBoard boards

parseBingoGame :: [String] -> Maybe BingoGame
parseBingoGame = fromLines
  where
    fromLines (xs : xss) = Just $ BingoGame (nums xs) (boards [] xss)
    fromLines _ = Nothing
    nums line = fmap (read . unpack) (splitOn (pack ",") (pack line))
    boards acc [] = reverse acc
    boards acc xss = boards (asBoard rows : acc) remaining
      where
        rows = take 5 nextBoard
        remaining = drop 5 nextBoard
        nextBoard = dropWhile null xss
        asBoard = Board . map (map read . words)

part1 :: BingoGame -> Int
part1 g = fromMaybe (-1) $ runGame $ initialGame g

part2 :: BingoGame -> Int
part2 g = fromMaybe (-1) $ runGame' $ initialGame g
