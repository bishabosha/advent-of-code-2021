module Main where

import qualified Advent (runChallenge, readLinesAsInt)
import qualified Day01 (part1, part2)
import qualified Day02 (parseCommands, part1, part2)

day01 = Advent.runChallenge "day01" Advent.readLinesAsInt
day02 = Advent.runChallenge "day02" (Day02.parseCommands . lines)

main :: IO ()
main = do
  day01 "part1" Day01.part1
  day01 "part2" Day01.part2
  day02 "part1" Day02.part1
  day02 "part2" Day02.part2

