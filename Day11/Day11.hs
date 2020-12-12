module Day11 where

import Data.Bool
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V

-- https://github.com/haskelling/aoc2020/blob/main/11a.hs

data Seat = Floor | Empty | Occupied deriving (Eq, Show)

type Grid = Vector (Vector Seat)

type Coord = (Int, Int)

count :: Eq a => a -> [a] -> Int
count c = length . filter (== c)

converge :: Eq a => (a -> a) -> a -> a
converge f x = let x' = f x in if x' == x then x else converge f x'

parse :: String -> Grid
parse = V.fromList . map (V.fromList . map (bool Floor Empty . (== 'L'))) . lines

nbs :: [Coord]
nbs = [(-1, 1), (0, 1), (1, 1), (-1, 0), (1, 0), (-1, -1), (0, -1), (1, -1)]

get :: Grid -> Coord -> Coord -> Maybe Seat
get m (x, y) (dx, dy) = do
  row <- m V.!? (y + dy)
  row V.!? (x + dx)

map8nbs :: (Seat -> [Seat] -> Seat) -> Grid -> Grid
map8nbs f m = V.imap (\y v -> V.imap (\x i -> modify i (x, y)) v) m
  where
    modify i (x, y) = f i $ mapMaybe (get m (x, y)) nbs

step :: Grid -> Grid
step = map8nbs rule
  where
    rule Empty ns = if count Occupied ns == 0 then Occupied else Empty
    rule Occupied ns = if count Occupied ns >= 4 then Empty else Occupied
    rule Floor _ = Floor

part1 :: Grid -> Int
part1 m = sum $ map (count Occupied . V.toList) $ V.toList $ converge step m

map8los :: (Seat -> [Seat] -> Seat) -> Grid -> Grid
map8los f m = V.imap (\y v -> V.imap (\x i -> modify i (x, y)) v) m
  where
    modify i (x, y) = f i $ mapMaybe (getFirst (x, y)) nbs

    getFirst (x0, y0) (x, y) = do
      v <- get m (x0, y0) (x, y)
      if v == Floor then getFirst (x0 + x, y0 + y) (x, y) else return v

step2 :: Grid -> Grid
step2 = map8los rule
  where
    rule Empty ns = if count Occupied ns == 0 then Occupied else Empty
    rule Occupied ns = if count Occupied ns >= 5 then Empty else Occupied
    rule Floor _ = Floor

part2 :: Grid -> Int
part2 m = sum $ map (count Occupied . V.toList) $ V.toList $ converge step2 m

main :: IO ()
main = do
  input <- readFile "input.txt"
  let pinput = parse input
  print $ part1 pinput
  print $ part2 pinput

ex1 :: String
ex1 =
  "L.LL.LL.LL\n\
  \LLLLLLL.LL\n\
  \L.L.L..L..\n\
  \LLLL.LL.LL\n\
  \L.LL.LL.LL\n\
  \L.LLLLL.LL\n\
  \..L.L.....\n\
  \LLLLLLLLLL\n\
  \L.LLLLLL.L\n\
  \L.LLLLL.LL"
