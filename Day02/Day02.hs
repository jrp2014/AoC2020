{-# LANGUAGE RecordWildCards #-}

module Day02 where

import Data.List.Split (splitOn)

data Entry = Entry {atLeast :: Int, atMost :: Int, letter :: Char, password :: String} deriving (Show)

parse :: String -> [Entry]
parse s = parseLine <$> lines s

parseLine :: String -> Entry
parseLine s = Entry {atLeast = read lo, atMost = read hi, letter = c, password = p}
  where
    [limits, c : _, p] = words s
    [lo, hi] = splitOn "-" limits

countValid :: (a -> Bool) -> [a] -> Int
countValid p = length . filter p

validate :: Entry -> Bool
validate Entry {..} = count >= atLeast && count <= atMost
  where
    count = countValid (== letter) password

validate2 :: Entry -> Bool
validate2 Entry {..} = 1 == countValid (== letter) [password !! (atLeast -1), password !! (atMost -1)]

part1 :: [Entry] -> Int
part1 = countValid validate

part2 :: [Entry] -> Int
part2 = countValid validate2

main :: IO ()
main = do
  input <- readFile "input.txt"
  let pinput = parse input
  print $ part1 pinput
  print $ part2 pinput

ex1 :: String
ex1 =
  "1-3 a: abcde\n\
  \1-3 b: cdefg\n\
  \2-9 c: ccccccccc"
