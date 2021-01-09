-- https://abhinavsarkar.net/posts/type-level-haskell-aoc7/
-- start snippet imports
{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, UndecidableInstances #-}
module AoC7 where

import Data.Proxy
import Data.Symbol.Ascii 
import GHC.TypeLits
import Prelude hiding (words, reverse)
-- end snippet imports

-- start snippet words
words :: [String] -> [String]
words s = reverse $ words2 [] s

words2 :: [String] -> [String] -> [String]
words2 acc []        = acc
words2 [] (" ":xs)   = words2 [] xs
words2 [] (x:xs)     = words2 [x] xs
words2 acc (" ":xs)  = words2 ("":acc) xs
words2 (a:as) (x:xs) = words2 ((a ++ x):as) xs

reverse :: [a] -> [a]
reverse l =  rev l []

rev :: [a] -> [a] -> [a]
rev = foldl (flip (:))
--rev []     a = a
--rev (x:xs) a = rev xs (x:a)
-- end snippet words

-- start snippet words-tf
type family Rev (acc :: [Symbol]) (chrs :: [Symbol]) :: [Symbol] where
  Rev '[] a = a
  Rev (x : xs) a = Rev xs (x : a)

type family Reverse (chrs :: [Symbol]) :: [Symbol] where
  Reverse l = Rev l '[]

type family Words2 (acc :: [Symbol]) (chrs :: [Symbol]) :: [Symbol] where
  Words2 acc '[]           = acc
  Words2 '[] (" " : xs)    = Words2 '[] xs
  Words2 '[] (x : xs)      = Words2 '[x] xs
  Words2 acc (" " : xs)    = Words2 ("" : acc) xs
  Words2 (a : as) (x : xs) = Words2 (AppendSymbol a x : as) xs

type family Words (chrs :: [Symbol]) :: [Symbol] where
  Words s = Reverse (Words2 '[] s)
-- end snippet words-tf

-- start snippet rules
type Rules = [
    "light red bags contain 1 bright white bag, 2 muted yellow bags."
  , "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
  , "bright white bags contain 1 shiny gold bag."
  , "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
  , "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
  , "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
  , "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
  , "faded blue bags contain no other bags."
  , "dotted black bags contain no other bags."
  ]
-- end snippet rules

-- start snippet bag
data Bag = EmptyBag Symbol | FilledBag Symbol [(Nat, Symbol)]
-- end snippet bag

-- start snippet parse
type family Parse (wrds :: [Symbol]) :: Bag where
  Parse (color1 : color2 : "bags" : "contain" : rest) =
    Parse2 (AppendSymbol color1 (AppendSymbol " " color2)) rest

type family Parse2 (color :: Symbol) (wrds :: [Symbol]) :: Bag where
  Parse2 color ("no" : _) = EmptyBag color
  Parse2 color rest = FilledBag color (Parse3 rest)

type family Parse3 (wrds :: [Symbol]) :: [(Nat, Symbol)] where
  Parse3 '[] = '[]
  Parse3 (count : color1 : color2 : _ : rest) =
    ('(ReadNat count, AppendSymbol color1 (AppendSymbol " " color2)) : Parse3 rest)
-- end snippet parse

-- start snippet parse-rules
type family ParseRules (rules :: [Symbol]) :: [Bag] where
  ParseRules '[] = '[]
  ParseRules (rule : rest) = (Parse (Words (ToList rule)) : ParseRules rest)

type Bags = ParseRules Rules
-- end snippet parse-rules

-- start snippet lookup
type family LookupBag (color :: Symbol) (bags :: [Bag]) :: Bag where
  LookupBag color '[] = TypeError (Text "Unknown color: " :<>: ShowType color)
  LookupBag color (EmptyBag color' : rest) =
    LookupBag2 color (CmpSymbol color color') (EmptyBag color') rest
  LookupBag color (FilledBag color' contained : rest) =
    LookupBag2 color (CmpSymbol color color') (FilledBag color' contained) rest

type family LookupBag2 (color :: Symbol)
                       (order :: Ordering)
                       (bag :: Bag)
                       (rest :: [Bag]) :: Bag where
  LookupBag2 _ EQ bag _ = bag
  LookupBag2 color _ _ rest = LookupBag color rest
-- end snippet lookup

-- start snippet bagcount
type family BagCount (color :: Symbol) :: Nat where
  BagCount color = BagCount2 (LookupBag color Bags)

type family BagCount2 (bag :: Bag) :: Nat where
  BagCount2 (EmptyBag _) = 0
  BagCount2 (FilledBag _ bagCounts) = BagCount3 bagCounts

type family BagCount3 (a :: [(Nat, Symbol)]) :: Nat where
  BagCount3 '[] = 0
  BagCount3 ( '(n, bag) : as) =
    n + n GHC.TypeLits.* BagCount2 (LookupBag bag Bags) + BagCount3 as
-- end snippet bagcount
