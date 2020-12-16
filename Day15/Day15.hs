{-# LANGUAGE BangPatterns #-}
module Day15 where

import Data.List (elemIndex, unfoldr)
import Data.Maybe (fromMaybe)

a181391 :: Int -> [Int] -> Int
a181391 n xs = a181391_list xs !! (n -1)

a181391_list :: [Int] -> [Int]
a181391_list xs = xs ++ unfoldr g (reverse xs)
  where
    g :: [Int] -> Maybe (Int, [Int])
    g xs = Just (m, m : xs)
      where
        !m = 1 + fromMaybe (-1) (elemIndex (head xs) $ tail xs)

main :: IO ()
main = do
  print $ a181391 2020 [15, 5, 1, 4, 7, 0]
