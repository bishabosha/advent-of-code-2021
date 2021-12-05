module Day02 (parseCommands, part1, part2) where

import Advent (capitalize)
import Control.Applicative (liftA2)
import Control.Monad (sequence)
import Text.Read (readEither, readMaybe, readPrec)

data Command = Forward | Down | Up
  deriving (Read)

data Move = Move {command :: Command, amount :: Int}

data Position = Position {depth :: Int, horizontal :: Int}

data WithAim = WithAim {pos :: Position, aim :: Int}

instance Read Move where
  readPrec = do
    c <- readPrec
    Move c <$> readPrec

parseCommands :: [String] -> Maybe [Move]
parseCommands = traverse $ readMaybe . capitalize

nextPosition :: Position -> Move -> Position
nextPosition (Position d h) (Move Forward i) = Position d (h + i)
nextPosition (Position d h) (Move Down i) = Position (d + i) h
nextPosition (Position d h) (Move Up i) = Position (d - i) h

nextWithAim :: WithAim -> Move -> WithAim
nextWithAim (WithAim (Position d h) a) (Move Forward i) = WithAim (Position (d + a * i) (h + i)) a
nextWithAim (WithAim p a) (Move Down i) = WithAim p (a + i)
nextWithAim (WithAim p a) (Move Up i) = WithAim p (a - i)

prodPos :: Position -> Int
prodPos (Position d h) = d * h

part1 :: [Move] -> Int
part1 = prodPos . foldl nextPosition p'
  where
    p' = Position 0 0

part2 :: [Move] -> Int
part2 = prodPos . pos . foldl nextWithAim wa'
  where
    wa' = WithAim (Position 0 0) 0
