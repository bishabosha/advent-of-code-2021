module Day02 (parseCommands, part1, part2) where

import Advent (capitalized)
import Control.Monad (sequence)
import Text.Read (readEither, readMaybe)

data Command = Forward | Down | Up
  deriving (Show, Eq, Enum, Read)

data Position = Position {depth :: Int, horizontal :: Int}
  deriving (Show, Eq)

data WithAim = WithAim {pos :: Position, aim :: Int}
  deriving (Show, Eq)

parseCommands :: [String] -> Maybe [(Command, Int)]
parseCommands = mapM parsed
  where
    parsed = parse . words
    parse [c, i] = do
      c' <- readMaybe (capitalized c)
      i' <- readMaybe i
      return (c', i')
    parse _ = Nothing

nextPosition :: Position -> Command -> Int -> Position
nextPosition (Position d h) Forward i = Position d (h + i)
nextPosition (Position d h) Down i = Position (d + i) h
nextPosition (Position d h) Up i = Position (d - i) h

nextWithAim :: WithAim -> Command -> Int -> WithAim
nextWithAim (WithAim (Position d h) a) Forward i = WithAim (Position (d + a * i) (h + i)) a
nextWithAim (WithAim p a) Down i = WithAim p (a + i)
nextWithAim (WithAim p a) Up i = WithAim p (a - i)

run :: (p -> c -> i -> p) -> p -> [(c, i)] -> p
run f = foldl (\p (c, i) -> f p c i)

prodPos :: Position -> Int
prodPos (Position d h) = d * h

part1 :: [(Command, Int)] -> Int
part1 = prodPos . run nextPosition p'
  where
    p' = Position 0 0

part2 :: [(Command, Int)] -> Int
part2 = (prodPos . pos) . run nextWithAim wa'
  where
    wa' = WithAim (Position 0 0) 0
