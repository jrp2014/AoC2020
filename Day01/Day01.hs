module Day01 where

import Data.List (tails)

ex1 :: [Int]
ex1 = [1721, 979, 366, 299, 675, 1456]

parse :: String -> [Int]
parse s = read <$> lines s

part1 :: [Int] -> Int
part1 expenses = head [x * y | x : xs <- tails expenses, y <- xs, x + y == 2020]

part2 :: [Int] -> Int
part2 expenses = head [x * y * z | x : xs <- tails expenses, y : ys <- tails xs, z <- ys, x + y + z == 2020]

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 (parse input)
  print $ part2 (parse input)
