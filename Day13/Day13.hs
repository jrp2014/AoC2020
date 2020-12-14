module Day13 where

-- https://github.com/glguy/advent2020/blob/master/execs/Day13.hs
-- https://github.com/haskelling/aoc2020/blob/main/13b.hs

import Data.List (foldl', foldl1')
import Data.List.Split (splitOn)

parse :: String -> (Integer, [Integer])
parse s = (read timestamp, read <$> filter (/= "x") (splitOn "," busIds))
  where
    [timestamp, busIds] = lines s

target :: Integer -> Integer -> Integer
target t b = b - t `mod` b

part1 :: (Integer, [Integer]) -> Integer
part1 (t, busIds) = uncurry (*) $ minimum [(target t busId, busId) | busId <- busIds]

data Mod = Mod Integer Integer deriving (Show, Read, Eq, Ord)

-- | Chinese Remainer Theorem
crt :: Mod -> Mod -> Mod
crt (Mod x m) (Mod y n) =
  let t = modinv (Mod n m) * x * n + modinv (Mod m n) * y * m
      m' = m * n
      x' = t `rem` m'
   in Mod x' m'
  where
    modinv (Mod x m) = let (_, x', _) = exteuc (Mod x m) in x' `rem` m
    exteuc (Mod 0 m) = (m, 0, 1)
    exteuc (Mod x m) =
      let (g, y, x') = exteuc (Mod (m `rem` x) x)
       in (g, x' - m `quot` x * y, y)

part2 :: [[Char]] -> Integer
part2 [_, bs] = let (Mod x m) = foldl1' crt (fst bs') in ((m - x) `rem` m)
  where
    bs' :: ([Mod], Integer)
    bs' = foldl' g ([], 0) $ map readBus $ splitOn "," bs

    g :: ([Mod], Integer) -> Maybe Integer -> ([Mod], Integer)
    g (xs, j) Nothing = (xs, j + 1)
    g (xs, j) (Just n) = (xs ++ [Mod j n], j + 1)

    readBus :: String -> Maybe Integer
    readBus "x" = Nothing
    readBus s = Just $ read s

main :: IO ()
main = do
  input <- readFile "input.txt"
  let pinput = parse input
  print pinput
  print $ part1 pinput
  print $ part2 $ lines input
