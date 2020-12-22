module Day21 where

import Data.Either (rights)
import Data.List (intercalate, sort, sortOn)
import Data.Map as Map (Map, assocs, elems, fromListWith)
import Data.Set as Set
  ( Set,
    delete,
    fromList,
    intersection,
    size,
    toList,
    unions,
    (\\),
  )
import Text.Parsec
  ( char,
    letter,
    many1,
    parse,
    sepEndBy,
    sepEndBy1,
    space,
    string,
  )
import Text.Parsec.String (Parser)

type Ingredient = String

type Ingredients = Set.Set String

type Allergen = String

type Allergens = [String]

data Food = Food {ingredients :: Ingredients, allergens :: Allergens}
  deriving (Show)

parseFood :: Parser Food
parseFood = do
  is <- many1 letter `sepEndBy` space
  _ <- string "(contains "
  as <- many1 letter `sepEndBy1` string ", "
  _ <- char ')'
  return $ Food (Set.fromList is) as

-- generates map from alergens to the ingredients that they might be in
mayBeIn :: [Food] -> Map Allergen Ingredients
mayBeIn foods =
  Map.fromListWith
    Set.intersection
    [(a, is) | Food is as <- foods, a <- as]

-- ingredients that may contain an allergen
mayContainAllergenIngredients :: [Food] -> Ingredients
mayContainAllergenIngredients = Set.unions . Map.elems . mayBeIn

-- all possible ingregients
allIngredients :: [Food] -> Ingredients
allIngredients foods = Set.unions (ingredients <$> foods)

-- ingredients that can't possibly contain an allergen
allergenFreeIngredients :: [Food] -> Ingredients
allergenFreeIngredients foods =
  allIngredients foods Set.\\ mayContainAllergenIngredients foods

-- count the number of times that allergen-free ingredients appear
part1 :: [Food] -> Int
part1 foods =
  sum
    [ length $ is `Set.intersection` allergenFreeIngredients foods
      | Food is _ <- foods
    ]

-- https://github.com/glguy/advent2020/blob/master/common/Advent.hs
uniqueAssignment ::
  (Ord a, Ord b) =>
  -- | each @a@ must map to one of the corresponding @b@
  [(a, Set b)] ->
  -- | assignments of @a@ and @b@ pairs
  [[(a, b)]]
uniqueAssignment m = case sortOn (Set.size . snd) m of
  [] -> [[]]
  (k, vs) : rest ->
    [(k, v) : soln | v <- Set.toList vs, soln <- uniqueAssignment (fmap (Set.delete v) <$> rest)]

part2 :: [Food] -> String
part2 =
  intercalate "," . map snd . sort . head . uniqueAssignment . Map.assocs . mayBeIn

main :: IO ()
main = do
  input <- readFile "input.txt"
  let foods = rights $ parse parseFood "" <$> lines input
  print $ part1 foods
  print $ part2 foods

ex1 :: String
ex1 =
  "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\n\
  \trh fvjkl sbzzf mxmxvkd (contains dairy)\n\
  \sqjhc fvjkl (contains soy)\n\
  \sqjhc mxmxvkd sbzzf (contains fish)"
