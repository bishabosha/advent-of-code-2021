module Day03 (part1, part2) where

import Control.Applicative (liftA2)
import Data.Bifunctor (first)
import Data.List (group, transpose)
import Text.Read (readMaybe)

data Bits = Bits {zeros :: Int, ones :: Int}
  deriving (Show, Eq)

mostCommon :: Bits -> Int
mostCommon (Bits z o) = if o >= z then 1 else 0

leastCommon :: Bits -> Int
leastCommon (Bits z o) = if z <= o then 0 else 1

categorise :: String -> [Bits]
categorise = fmap (\x -> if x == '0' then Bits 1 0 else Bits 0 1)

addBit :: Bits -> Bits -> Bits
addBit b1 b2 = Bits (zeros b1 + zeros b2) (ones b1 + ones b2)

part1 :: [String] -> Int
part1 xs = gamma summary * epsilon summary
  where
    gamma = toDec . fmap mostCommon
    epsilon = toDec . fmap leastCommon
    summary = sumBits <$> columns
    columns = categorised xs
    categorised = fmap categorise . transpose
    sumBits = foldl addBit $ Bits 0 0
    toDec = foldl (\acc x -> acc * 2 + x) 0

part2 :: [String] -> Int
part2 xs = oxygen * co2
  where
    oxygen = iter (criteria mostCommon) xs
    co2 = iter (criteria leastCommon) xs
    criteria f s = (== f (summary s))
    summary = sumBits . categorise
    sumBits = foldl addBit $ Bits 0 0

iter :: (String -> Int -> Bool) -> [String] -> Int
iter f xs = iter' f initCol withParsed
  where
    iter' f s xs = process $ filtered s xs
    initCol = fmap head xs
    withParsed = zip xs $ fmap fromBinary xs
    fromBinary = foldl (\acc c -> acc * 2 + fromChar c) 0
    filtered s xs = filter (firstChar $ f s) xs
    firstChar f (c : _, _) = f (fromChar c)
    firstChar _ _ = False
    fromChar c = if c == '0' then 0 else 1
    process [] = undefined -- should never happen if the input is valid
    process [(_, r)] = r
    process xs = iter' f (firstCol xs) $ dropCol xs
    firstCol xs = fmap (head . tail . fst) xs
    dropCol xs = fmap (first tail) xs