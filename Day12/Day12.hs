module Day12 where

import Data.List (foldl')

type Coord = (Int, Int)

type Position = Coord

type Waypoint = Coord

type Ship = (Position, Waypoint)

manhatten :: Coord -> Int
manhatten (x, y) = abs x + abs y

dir :: Char -> Coord
dir 'E' = (1, 0)
dir 'N' = (0, 1)
dir 'W' = (-1, 0)
dir 'S' = (0, -1)

instance (Num a, Num b) => Num (a, b) where
  (x, y) + (u, v) = (x + u, y + v)
  (x, y) * (u, v) = (x * u, y * v)
  negate (x, y) = (negate x, negate y)
  fromInteger x = (fromInteger x, 0)
  abs (x, y) = (abs x, abs y)
  signum (x, y) = (signum x, signum y)

(*$) :: Int -> Coord -> Coord
n *$ (x, y) = (n * x, n * y)

rot :: Coord -> Coord
rot (x, y) = (- y, x)

rotn :: Int -> Coord -> Coord
rotn 0 = id
rotn n = rot . rotn ((n - 1) `mod` 4)

move :: Ship -> String -> Ship
move ship (action : argument) = move' ship action (read argument :: Int)
  where
    move' :: Ship -> Char -> Int -> Ship
    move' (p, w) 'F' n = (p + (n *$ w), w)
    move' (p, w) 'L' n = (p, rotn (n `div` 90) w)
    move' (p, w) 'R' n = move' (p, w) 'L' (- n)
    move' (p, w) w' n = (p + (n *$ dir w'), w)

part1 :: [String] -> Int
part1 xs = manhatten $ fst $ foldl' move (0, dir 'E') xs

move2 :: Ship -> String -> Ship
move2 ship (action : argument) = move' ship action (read argument :: Int)
  where
    move' :: Ship -> Char -> Int -> Ship
    move' (p, w) 'F' n = (p + (n *$ w), w)
    move' (p, w) 'L' n = (p, rotn (n `div` 90) w)
    move' (p, w) 'R' n = move' (p, w) 'L' (- n)
    move' (p, w) w' n = (p, w + (n *$ dir w'))

part2 :: [String] -> Int
part2 xs = manhatten $ fst $ foldl' move2 (0, (10, 1)) xs

main :: IO ()
main = do
  input <- readFile "input.txt"
  let pinput = lines input
  print $ part1 pinput
  print $ part2 pinput
