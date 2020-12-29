module Day16 where

-- https://github.com/haskelling/aoc2020/blob/main/16a.hs
-- https://github.com/glguy/advent2020/blob/master/execs/Day16.hs

import Data.List (isPrefixOf, sortOn, transpose)
import Data.List.Split (splitOn)
import qualified Data.Set as Set

type Ticket = Int

type Range = (Int, Int)

type Field = (String, [Range])

fieldp :: String -> Field
fieldp s = (name, ranges)
  where
    [name, rs] = splitOn ": " s
    ranges = map ((\[x, y] -> (x, y)) . map read . splitOn "-") $ splitOn " or " rs

inRange :: Int -> Range -> Bool
inRange x (low, high) = low <= x && x <= high

match :: Field -> Int -> Bool
match (_, ranges) x = any (inRange x) ranges

ticketp :: String -> [Ticket]
ticketp = map read . splitOn ","

parse :: String -> ([Field], [Ticket], [[Ticket]])
parse s = (map fieldp fields, ticketp yourTickets, map ticketp nearbyTickets)
  where
    [fields, [_yt, yourTickets], _nt : nearbyTickets] = splitOn [""] $ lines s

uniqueAssignment ::
  (Ord a, Ord b) =>
  -- | each @a@ must map to one of the corresponding @b@
  [(a, Set.Set b)] ->
  -- | assignments of @a@ and @b@ pairs
  [[(a, b)]]
uniqueAssignment m =
  case sortOn (Set.size . snd) m of
    [] -> [[]]
    (k, vs) : rest ->
      [ (k, v) : soln
        | v <- Set.toList vs,
          soln <- uniqueAssignment (fmap (Set.delete v) <$> rest)
      ]

main :: IO ()
main = do
  -- let input = ex2
  input <- readFile "input.txt"
  let (fields, yourTickets, nearbyTickets) = parse input
  -- Part1: sum of nearby tickets that match no fields
  print $ sum [t | ts <- nearbyTickets, t <- ts, not (any (`match` t) fields)]
  -- Part 2
  -- A good ticket is a nearby ticket that matches at least one field
  let goodTickets = [ts | ts <- nearbyTickets, all (\t -> any (`match` t) fields) ts]
  let possibleFields column = Set.fromList [fst field | field <- fields, all (match field) column]
  let allCandidates = [possibleFields column | column <- transpose goodTickets]
  -- pair up your ticket's field values with the candidate field names
  let constraints = zip yourTickets allCandidates
  print $
    product
      [p | (p, name) <- head (uniqueAssignment constraints), "departure" `isPrefixOf` name]

ex1 :: String
ex1 =
  "class: 1-3 or 5-7\n\
  \row: 6-11 or 33-44\n\
  \seat: 13-40 or 45-50\n\
  \\n\
  \your ticket:\n\
  \7,1,14\n\
  \\n\
  \nearby tickets:\n\
  \7,3,47\n\
  \40,4,50\n\
  \55,2,20\n\
  \38,6,12"

ex2 :: String
ex2 =
  "class: 0-1 or 4-19\n\
  \row: 0-5 or 8-19\n\
  \seat: 0-13 or 16-19\n\
  \\n\
  \your ticket:\n\
  \11,12,13\n\
  \\n\
  \nearby tickets:\n\
  \3,9,18\n\
  \15,1,5\n\
  \5,14,9"
