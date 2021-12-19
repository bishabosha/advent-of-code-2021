module Main where

import qualified Advent (runChallenge, readLinesAsInt)
import qualified Day01 (part1, part2)
import qualified Day02 (parseCommands, part1, part2)
import qualified Day03 (part1, part2)
import qualified Day04 (parseBingoGame, part1, part2)
import qualified Day05 (parseLines, part1, part2)
import qualified Day06 (parseFish, part1, part2)

day01 = Advent.runChallenge "day01" Advent.readLinesAsInt
day02 = Advent.runChallenge "day02" (Day02.parseCommands . lines)
day03 = Advent.runChallenge "day03" (Just . lines)
day04 = Advent.runChallenge "day04" (Day04.parseBingoGame . lines)
day05 = Advent.runChallenge "day05" (Day05.parseLines . lines)
day06 = Advent.runChallenge "day06" (Day06.parseFish)

main :: IO ()
main = do
  day01 "part1" Day01.part1
  day01 "part2" Day01.part2
  day02 "part1" Day02.part1
  day02 "part2" Day02.part2
  day03 "part1" Day03.part1
  day03 "part2" Day03.part2
  day04 "part1" Day04.part1
  day04 "part2" Day04.part2
  day05 "part1" Day05.part1
  day05 "part2" Day05.part2
  day06 "part1" Day06.part1
  day06 "part2" Day06.part2

