module Day01 (part1, part2) where

challenge :: Int -> [Int] -> Int
challenge n depths = length . filter (uncurry (<)) $ pairs
  where
    pairs = zip depths $ drop n depths

part1 :: [Int] -> Int
part1 = challenge 1

part2 :: [Int] -> Int
part2 = challenge 3