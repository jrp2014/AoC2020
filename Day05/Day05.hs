module Day05 where

import Data.List (foldl', sort)

seat :: String -> (Int, Int)
seat = foldl' f (0, 0)
  where
    f (r, c) 'L' = (r, 2 * c)
    f (r, c) 'R' = (r, 2 * c + 1)
    f (r, c) 'F' = (2 * r, c)
    f (r, c) 'B' = (2 * r + 1, c)

seatID :: String -> Int
seatID s = 8 * r + c
  where
    (r, c) = seat s

gap :: [Int] -> Int
gap (x : y : _) | x + 1 /= y = x + 1
gap (_ : xs) = gap xs

main :: IO ()
main = do
  input <- readFile "input.txt"
  let pinput = lines input
  let seatIDs = seatID <$> pinput
  print $ maximum seatIDs
  print $ gap (sort seatIDs)
