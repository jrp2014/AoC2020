{-# LANGUAGE TypeApplications #-}

module Day17 where

-- https://github.com/ephemient/aoc2020/blob/e07626eeab19cecb4ef396df576ae8401ffbf248/hs/src/Day17.hs

import Control.Monad (replicateM)
import qualified Data.Map as Map
  ( filterWithKey,
    fromListWith,
    keysSet,
  )
import Data.Monoid (Sum (Sum))
import Data.Set (Set)
import qualified Data.Set as Set
  ( fromDistinctAscList,
    member,
    toAscList,
  )

data Vec4 = Vec4 !Int !Int !Int !Int
  deriving (Eq, Ord, Show)

-- Create the set of live coordinates
parse :: String -> Set Vec4
parse input =
  Set.fromDistinctAscList
    [ Vec4 x y 0 0
      | (x, line) <- zip [0 ..] $ lines input,
        (y, '#') <- zip [0 ..] line
    ]

step :: Int -> Set Vec4 -> Set Vec4
step n s =
  Map.keysSet . Map.filterWithKey ok $
    Map.fromListWith -- (coord, frequency)
      (<>)
      [ (Vec4 (x + dx) (y + dy) (z + dz) (w + dw), Sum @Int 1)
        | Vec4 x y z w <- Set.toAscList s,
          dx : dy : dz : dw : _ <- (++ repeat 0) <$> replicateM n [-1 .. 1],
          (dx, dy, dz, dw) /= (0, 0, 0, 0)
      ]
  where
    ok _ (Sum 3) = True
    ok p (Sum 2) = p `Set.member` s
    ok _ _ = False

part1 :: String -> Int
part1 input = length $ iterate (step 3) (parse input) !! 6

part2 :: String -> Int
part2 input = length $ iterate (step 4) (parse input) !! 6

main :: IO ()
main = do
  input <- readFile "input.txt"
  --let input = ex1
  print $ part1 input
  print $ part2 input

ex1 :: String
ex1 =
  ".#.\n\
  \..#\n\
  \###"
