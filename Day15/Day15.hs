{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.IntMap.Strict as M hiding (drop)
import           Data.List          (elemIndex, mapAccumL, unfoldr)
import           Data.Maybe         (fromMaybe, maybe)

a181391 :: Int -> [Int] -> Int
a181391 n xs = a181391_list xs !! (n - 1)

a181391_list :: [Int] -> [Int]
a181391_list xs = xs ++ unfoldr g (reverse xs)
 where
  g :: [Int] -> Maybe (Int, [Int])
  g xs = Just (m, m : xs)
    where !m = 1 + fromMaybe (-1) (elemIndex (head xs) $ tail xs)


-- https://rosettagit.org/drafts/van-eck-sequence/#haskell
vanEck :: [Int] -> [Int]
vanEck xs = xs ++ snd
  (mapAccumL go (last xs, M.fromList $ zip (init xs) [1 ..]) [length xs ..])
 where
  go :: (Int, M.IntMap Int) -> Int -> ((Int, M.IntMap Int), Int)
  go (x, dct) i =
    let v = maybe 0 (i -) (M.lookup x dct) in ((v, M.insert x i dct), v)

startingNos :: [Int]
startingNos = [15, 5, 1, 4, 7, 0]
--startingNos = [0, 3, 6]

main :: IO ()
main = do
  print $ a181391 2020 startingNos
--  print $ take 20 $ a181391_list startingNos
  print $ vanEck startingNos !! (2020 - 1)
  print $ vanEck startingNos !! (30000000 - 1)
