module Day03 (part1) where

-- power consumption = gamma rate * epsilon rate

-- gamma rate = most common bit in position

import Data.Bits ()
import Text.Read (readMaybe)
import Control.Applicative (Applicative(liftA2))
import Data.List (group, transpose)

-- binaryDigits :: [String] -> Maybe [Int]
-- binaryDigits = traverse fromBinary
--   where fromBinary = foldl (\acc x -> liftA2 (+) acc (readMaybe [x])) (Just 0)

data Bits = Bits {zeros :: Int, ones :: Int}
  deriving (Show, Eq)

mostCommon :: Bits -> Int
mostCommon (Bits z o) = if z > o then 0 else 1

leastCommon :: Bits -> Int
leastCommon (Bits z o) = if z > o then 1 else 0

categorise :: String -> [Bits]
categorise = fmap (\x -> if x == '0' then Bits 1 0 else Bits 0 1)

addBit :: Bits -> Bits -> Bits
addBit (Bits z1 o1) (Bits z2 o2) = Bits (z1 + z2) (o1 + o2)

part1 :: [String] -> Int
part1 xs = gamma summary * epsilon summary
  where
    summary = reduce <$> categorised xs
    categorised = fmap categorise . transpose
    reduce = foldl addBit $ Bits 0 0
    gamma = toDec . fmap mostCommon
    epsilon = toDec . fmap leastCommon
    toDec = foldl (\acc x -> acc * 2 + x) 0
