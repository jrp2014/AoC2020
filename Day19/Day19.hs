module Day19 where

-- https://topaz.github.io/paste/#XQAAAQCrBQAAAAAAAAA2m8ixrhLu7YJFrd2FLde+PAG1Aui2yN36LC93WIQ2APMCiCS0U/oCzTqxk22sBYRzDRWsKEu4AaU2Rt4MtSBQB6g5emCgR4uBaS74+pybb5OVDQA/AWHSkahcfGvdzgBEH7WkxoPDBg7quCE5lU0gRXgHcmh1zMBwV5W5pn2/14wHeO1SnKuGeZjHXvUzqjn9B12lIvI7mOyLkJpSOODG4lsW/7jfWqJJ5DEEAYZB+1kwYBIq4p7sO0zqV+To78YwzWo6pkjQD/7NwqD+Qpacozueg3Q3w4qjMN8oi8LWBJP1YOU9Wuxh4ge8gNfArkh3EGP5WtfV4rk9NnQtkLUWi6PVyNOWwrDKsliSbyIOMgXYS04xf3mKiQl3DStRiH+Ci/OvAx9UiuNIMeHTUjGzjDmDb4K2skbData55rOe6FJcWxIWhqcyFz3BuaatomUHHolO2Lp+A8XFAfscNUQmi6Y+NQEW8JRLeT/mv7HV21bIAD9deRjGNCpdfK/CTpCq+UCfjKqugBrIt15RavDYOg1AZ146VZRG6YXdjmVYhwxkvnABBvaSPZYwo2sDaft5JdUp9J/gX10omtULwQ7WKMg/KuD5ZNAD/7b8+ND4DiqL3+qFZQVo8+1C6vhzLCIAJvA7CeLmXKGWi9rHaosNpPEwupvOyLcHRMAO5kJJQYjstW61YWaYmowBAkRBt2e4YgKkIBX1zKAq0Or6KXqsvem2hPkvgoG2Nw8Mn7nx85pDm+CGn4skSr+OfvH1lSmzNlrMPMd0Ix8yLbP6CdBZnezjshO3hZ7BPQExqgTB4Fwdrqxy/WvJ4rKqbRgj8k5dfXplH14gAp8vmXyD25jXUWW0HAPz4qVIj0QTolcD+SnTPK2EZzLpzngfI/DIIF/o0Met6I+hJBJvtR//88ZXdQ==
import Control.Monad (foldM)
import Data.IntMap as IM (IntMap, fromList, insert, (!))
import Data.List.Split (splitOn)

-- sum of products.  Could be tightened up
data Rule = And [Int] | Or [Rule] | Literal Char
  deriving (Show, Eq, Ord)

type Rules = IM.IntMap Rule

parseRule :: [String] -> Rule
parseRule [['\"', c, '\"']] = Literal c
parseRule rules = Or $ And . map read <$> splitOn ["|"] rules

parse :: String -> (IM.IntMap Rule, [String])
parse input = (rules, lines inputsStr)
  where
    [rulesStr, inputsStr] = splitOn "\n\n" input

    rules =
      IM.fromList
        [ (read idx, parseRule $ words pat)
          | line <- lines rulesStr,
            let [idx, pat] = splitOn ": " line
        ]

-- returns [] if a complete match is found
match :: Rules -> String -> Rule -> [String]
match _ "" (Literal _) = []
match _ (c' : rest) (Literal c)  = [rest | c' == c]
match rules str (And ns) = foldM (\s n -> match rules s (rules IM.! n)) str ns
match rules str (Or rs) = concatMap (match rules str) rs

matchRule0 :: (Rules, [String]) -> Int
matchRule0 (rules, inputs) =
  length
    [ input | input <- inputs, any null $ match rules input (rules IM.! 0) ]

part1 :: (Rules, [String]) -> Int
part1 = matchRule0

part2 :: (Rules, [String]) -> Int
part2 (rules, inputs) =
  matchRule0
    ( IM.insert  8 (Or [And [42], And [42, 8]])
    $ IM.insert 11 (Or [And [42, 31], And [42, 11, 31]])
    $ rules,
      inputs
    )

main :: IO ()
main = do
  (rules, inputs) <- parse <$> readFile "input.txt"
  print $ part1 (rules, inputs)
  print $ part2 (rules, inputs)

ex1 :: String
ex1 =
  "0: 4 1 5\n\
  \1: 2 3 | 3 2\n\
  \2: 4 4 | 5 5\n\
  \3: 4 5 | 5 4\n\
  \4: \"a\"\n\
  \5: \"b\"\n\
  \\n\
  \ababbb\n\
  \bababa\n\
  \abbbab\n\
  \aaabbb\n\
  \aaaabbb"
