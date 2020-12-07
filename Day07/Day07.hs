module Day07 where

import Data.Either ( rights )
import Data.List ( foldl' )
import Data.Map ( Map )
import qualified Data.Map as Map
    ( (!), fromList, insertWith, member, unionsWith )
import Text.Parsec
    ( char,
      digit,
      letter,
      string,
      many1,
      optional,
      sepBy1,
      (<|>),
      parse )
import Text.Parsec.String ( Parser )

-- https://github.com/glguy/advent2020/blob/master/execs/Day07.hs
-- https://github.com/haskelling/aoc2020/blob/main/7a.hs

type Bag = String

type Count = Int

type Rule = (Bag, [(Count, Bag)])

rule :: Parser Rule
rule = do
  b <- bag
  _ <- string " contain "
  bs <- (string "no other bags" >> return []) <|> (bags `sepBy1` string ", ")
  _ <- char '.'
  return (b, bs)

bag :: Parser Bag
bag = do
  d1 <- many1 letter
  _ <- char ' '
  d2 <- many1 letter
  _ <- string " bag"
  optional $ char 's'
  return $ d1 ++ ' ' : d2

bags :: Parser (Int, Bag)
bags = do
  n <- read <$> many1 digit
  _ <- char ' '
  b <- bag
  return (n, b)

-- http://blog.sigfpe.com/2006/11/from-l-theorem-to-spreadsheet.html
loeb :: Functor a => a (a x -> x) -> a x
--loeb x = fmap (\a -> a (loeb x)) x
loeb x = go where go = fmap ($ go) x

-- Count the number of elements in a foldable value that satisfy a predicate.
count :: Foldable t => (a -> Bool) -> t a -> Int
count p = foldl' (\acc x -> if p x then acc + 1 else acc) 0

transClosBags :: [Rule] -> Map Bag (Map Bag Count)
transClosBags rules = loeb (expand <$> Map.fromList rules)
  where
    expand :: [(Count, Bag)] -> Map Bag (Map Bag Count) -> Map Bag Count
    expand inside tc =
      Map.unionsWith
        (+)
        [(n *) <$> Map.insertWith (+) b 1 (tc Map.! b) | (n, b) <- inside]

main :: IO ()
main = do
  input <- readFile "input.txt"
  let pinput = rights $ map (parse rule "") (lines input)
  let tc = transClosBags pinput
  print (count (Map.member "shiny gold") tc)
  print (sum (tc Map.! "shiny gold"))
