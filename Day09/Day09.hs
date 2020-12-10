module Day09 where

import           Data.Foldable (asum)
import           Data.List     (inits, tails, transpose)
import           Data.Maybe    (fromJust)

-- NB this doesn't omit the short windows at the end
slidingWindows :: Int -> [a] -> [[a]]
slidingWindows n = transpose . take n . tails

isInvalidRun :: [Int] -> Maybe Int
isInvalidRun xs = isInvalid (last xs) (init xs)
  where
    isInvalid :: Int -> [Int] -> Maybe Int
    isInvalid n [] = Just n
    isInvalid x (y : ys) = if (x - y) `notElem` ys then isInvalid x ys else Nothing

part1 :: Int -> [Int] -> Int
part1 n = fromJust . asum . map isInvalidRun . slidingWindows (n + 1)


-- https://github.com/haskelling/aoc2020/blob/main/9b.hs
part2 :: Int -> [Int] -> Int
part2 n xs = maximum xs' + minimum xs'
  where
    xs' = h n xs
    
    -- contiguous sublists of xs that sum to n
    h n xs = head $ filter ((==n) . sum) $ concatMap tails $ inits xs


main :: IO ()
main = do
  input <- readFile "input.txt"
  let pinput = map read (lines input) :: [Int]
  let invalidNum = part1 25 pinput
  print invalidNum
  print $ part2 invalidNum pinput


ex1 :: [Int]
ex1 =
  [ 35,
    20,
    15,
    25,
    47,
    40,
    62,
    55,
    65,
    95,
    102,
    117,
    150,
    182,
    127,
    219,
    299,
    277,
    309,
    576
  ]
