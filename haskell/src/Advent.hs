module Advent (inputChallenge, readLinesAsInt, runChallenge, capitalize) where

import qualified Data.Char as Char
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

inputChallenge :: String -> IO String
inputChallenge name = readFile $ "inputs/" ++ name

readLinesAsInt :: String -> Maybe [Int]
readLinesAsInt = mapM readMaybe . lines

capitalize :: String -> String
capitalize [] = []
capitalize (x : xs) = Char.toUpper x : xs

runChallenge :: Show b => String -> (String -> Maybe a) -> String -> (a -> b) -> IO ()
runChallenge name parser part challenge = do
  input <- inputChallenge name
  putStrLn $ maybe parseErr (showRes . challenge) (parser input)
  where
    parseErr = "Failed to parse input for challenge " ++ fullName
    showRes = (\r -> concat [fullName, ": ", r]) . show
    fullName = concat [name, "-", part]