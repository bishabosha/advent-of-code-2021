module Main where

import qualified Advent (runChallenge, readLinesAsInt)
import qualified Day01 (part1, part2)

day01 = Advent.runChallenge "day01" Advent.readLinesAsInt

main :: IO ()
main = do
  day01 "part1" Day01.part1
  day01 "part2" Day01.part2
