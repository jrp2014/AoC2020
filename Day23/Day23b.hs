module Main where

import           Data.Char                      ( digitToInt )
import qualified Data.IntMap.Strict            as M
import           Data.List
import           Data.Maybe

-- https://github.com/waltont8/AdventOfCode2020/blob/main/day23_part02_Lib.hs
-- runs in about 30s when compiled -O2

type DB = M.IntMap Int

main :: IO ()
main = do
  let rawData = map digitToInt "925176834" ++ [10 .. 1000000]
  let cur        = head rawData
  let input = M.fromList $ zip rawData (tail rawData ++ [10])
  let tenmillion = snd (iterate move (cur, input) !! 10000000)
  let p1         = fromJust $ M.lookup 1 tenmillion
  let p2         = fromJust $ M.lookup p1 tenmillion
  print $ p1 * p2

move :: (Int, DB) -> (Int, DB)
move (current, db) = (rest, db''')
 where
  a        = fromJust $ M.lookup current db
  b        = fromJust $ M.lookup a db
  c        = fromJust $ M.lookup b db
  rest     = fromJust $ M.lookup c db
  db'      = M.insert current rest db
  insAfter = dest current
  oldValue = fromJust $ M.lookup insAfter db'
  db''     = M.insert insAfter a db'
  db'''    = M.insert c oldValue db''

  dest _cur
    | (_cur - 1) < sminimum a b c = smaximum a b c
    | ((_cur - 1) /= a) && ((_cur - 1) /= b) && ((_cur - 1) /= c) = _cur - 1
    | otherwise                   = dest (_cur - 1)

-- from 0
smaximum :: Int -> Int -> Int -> Int
smaximum a b c = maximum ([999997, 999998, 999999, 1000000] \\ [a, b, c])

sminimum :: Int -> Int -> Int -> Int
sminimum a b c = minimum ([1, 2, 3, 4] \\ [a, b, c])

