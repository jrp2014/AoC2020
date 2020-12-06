module Day06 where

import Data.List (intersect, union)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let pinput = (map lines . splitOn "\n\n") input
  print $ sum (map (length . foldr union []) pinput)
  print $ sum (map (length . foldr1 intersect) pinput)
