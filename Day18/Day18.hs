module Day18 where

import Data.Either ( rights )
import Text.Parsec
    ( digit, string, between, choice, many1, parse, ParseError )
import Text.Parsec.String ( Parser )

-- https://github.com/mstksg/advent-of-code-2020/blob/master/src/AOC/Challenge/Day18.hs

number :: Parser Int
number = read <$> many1 digit

parens :: Parser a -> Parser a
parens = between (string "(") (string ")")

parseTerm :: Parser Int
parseTerm = choice [ number, parens parseExpr ]

parseExpr :: Parser Int
parseExpr = do
    leftTerm <- parseTerm
    doNext leftTerm
  where
    doNext :: Int -> Parser Int
    doNext acc = choice
        [ do
              _ <- string "*"
              rightTerm <- parseTerm
              doNext (acc * rightTerm)
        , do
              _ <- string "+"
              rightTerm <- parseTerm
              doNext (acc + rightTerm)
        , pure acc
        ]

-- strip spaces to get a list of tokens
part1 :: String -> Either ParseError Int
part1 = parse parseExpr "" . filter (/= ' ')

parseBottom2 :: Parser Int
parseBottom2 = choice [ number, parens parseTop2 ]

parseMiddle2 :: Parser Int
parseMiddle2 = do
    leftOfOp <- parseBottom2
    doNext leftOfOp
  where
    doNext :: Int -> Parser Int
    doNext acc = choice [ do
                              _ <- string "+"
                              rightOfOp <- parseBottom2
                              doNext (acc + rightOfOp)
                        , pure acc
                        ]

parseTop2 :: Parser Int
parseTop2 = do
    leftOfOp <- parseMiddle2
    doNext leftOfOp
  where
    doNext :: Int -> Parser Int
    doNext acc = choice [ do
                              _ <- string "*"
                              rightOfOp <- parseMiddle2
                              doNext (acc * rightOfOp)
                        , pure acc
                        ]

part2 :: String -> Either ParseError Int
part2 = parse parseTop2 "" . filter (/= ' ')

main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    print . sum . rights $ part1 <$> input
    print . sum . rights $ part2 <$> input
