{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day04 where

import Control.Monad ( guard )
import Data.Char ( isAlpha, isDigit, isHexDigit, isSpace )
import Data.List ( delete, foldl', sort )
import Data.Maybe ( isJust )
import Text.Parsec
    ( satisfy, string, manyTill, sepEndBy, many, parse, ParseError )
import Text.Parsec.String ( Parser )
import Text.Read ( readMaybe )

-- https://github.com/glguy/advent2020/blob/master/execs/Day04.hs
type Entry = (String, String)

type Passport = [Entry]

entry :: Parser Entry
entry =
  (,) <$> manyTill (satisfy isAlpha) (string ":")
    <*> many
      (satisfy (not . isSpace))

parse :: String -> [Passport]
parse s = case parse' s of
  Left err -> error (show err)
  Right xs -> xs
  where
    parse' :: String -> Either ParseError [[Entry]]
    parse' = Text.Parsec.parse ((entry `sepEndBy` satisfy isSpace) `sepEndBy` string "\n") ""

-- | Count the number of elements in a foldable value that satisfy a predicate.
countValid :: Foldable t => (a -> Bool) -> t a -> Int
countValid p = foldl' (\acc x -> if p x then acc + 1 else acc) 0

-- required entries
reqEntries :: [String]
reqEntries = sort ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

-- all entries other perhaps than "cid" are present
complete :: Passport -> Bool
complete x = reqEntries == sort (delete "cid" (map fst x))

part1 :: [Passport] -> Int
part1 = countValid complete

--

inRange :: Ord a => a -> a -> a -> Bool
inRange lo hi x = lo <= x && x <= hi

valid :: Passport -> Bool
valid x = isJust
  do
    guard . inRange (1920 :: Int) 2002 =<< readMaybe =<< lookup "byr" x
    guard . inRange (2010 :: Int) 2020 =<< readMaybe =<< lookup "iyr" x
    guard . inRange (2020 :: Int) 2030 =<< readMaybe =<< lookup "eyr" x

    (hgtStr, hgtU) <- span isDigit <$> lookup "hgt" x
    hgt :: Int <- readMaybe hgtStr
    guard case hgtU of
      "cm" -> inRange 150 193 hgt
      "in" -> inRange 59 76 hgt
      _ -> False

    '#' : cs <- lookup "hcl" x
    guard (length cs == 6 && all isHexDigit cs)

    ecl <- lookup "ecl" x
    guard (ecl `elem` words "amb blu brn gry grn hzl oth")

    pid <- lookup "pid" x
    guard (length pid == 9 && all isDigit pid)

part2 :: [Passport] -> Int
part2 = countValid valid

main :: IO ()
main = do
  input <- readFile "input.txt"
  let pinput = Day04.parse input
  print $ part1 pinput
  print $ part2 pinput

ex1 :: String
ex1 =
  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n\
  \byr:1937 iyr:2017 cid:147 hgt:183cm\n\
  \\n\
  \iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\n\
  \hcl:#cfa07d byr:1929\n\
  \\n\
  \hcl:#ae17e1 iyr:2013\n\
  \eyr:2024\n\
  \ecl:brn pid:760753108 byr:1931\n\
  \hgt:179cm\n\
  \\n\
  \hcl:#cfa07d eyr:2025 pid:166559648\n\
  \iyr:2011 ecl:brn hgt:59in"
