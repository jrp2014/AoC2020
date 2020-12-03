{-# LANGUAGE BangPatterns #-}

module Day03 where

-- https://github.com/mstksg/advent-of-code-2020/blob/master/src/AOC/Challenge/Day03.hs
import Data.Char (isSpace)

type Coord = (Int, Int)

type Step = Int

width :: Int
width = 31 -- 11, for test case

-- concatenates the grid lines into a single String
parse :: String -> String
parse = filter (not . isSpace)

validCoord :: Step -> Step -> Coord -> Bool
validCoord dx dy (x, y) = r == 0 && (dx * i) `mod` width == x
  where
    (i, r) = y `divMod` dy

countTrue :: (a -> Bool) -> [a] -> Int
countTrue p = length . filter p

countTree :: Step -> Step -> String -> Int
countTree dx dy = countTrue (uncurry isTree) . zip (toCoord <$> [0 ..])
  where
    isTree coord char = char == '#' && validCoord dx dy coord

    toCoord :: Int -> Coord
    toCoord i = (x, y)
      where
        (!y, !x) = i `divMod` width

part1 :: String -> Int
part1 = countTree 3 1 . parse

part2 :: String -> Int
part2 s =
  product
    [ countTree 1 1 ps,
      countTree 3 1 ps,
      countTree 5 1 ps,
      countTree 7 1 ps,
      countTree 1 2 ps
    ]
  where
    ps = parse s

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input

ex1 :: String
ex1 =
  "..##.......\n\
  \#...#...#..\n\
  \.#....#..#.\n\
  \..#.#...#.#\n\
  \.#...##..#.\n\
  \..#.##.....\n\
  \.#.#.#....#\n\
  \.#........#\n\
  \#.##...#...\n\
  \#...##....#\n\
  \.#..#...#.#\n"
