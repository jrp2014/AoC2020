module Day10 where

import Data.List (group, sort)

makeList :: [Int] -> [Int]
makeList list = 0 : sort list ++ [maximum list + 3]

diffs :: [Int] -> [Int]
diffs xs@(_ : ys) = zipWith (-) ys xs

part1 :: [Int] -> Int
part1 = product . map length . group . sort . diffs

-- https://topaz.github.io/paste/#XQAAAQBjBAAAAAAAAAA2m8ixrhLu7YJFrd2FLde+PAG1Aui2yN36LC93WIQ2APMCiCS0L/ERCZO1Ub196UifbJLLVlbSvYxOwAjYDrQYBebXdg1tlZnkpCddIzEyACJhqVaVyfEHliHhHTo43RuH6MUu0/w7UwQVDRxlqCPYNFIpYzS1qKJg/dS0LL2ErMa11sDhjnpt6/o51Ih9QtTRsUzO6GnqZLny1iBhMg0nSw2pDcXMryFYJwvLEhhnOA2VNJhWM1Gn+OCdev9TOOzuOQowvMP36UPzxmVWAUAZjDS15RcLbFFeojz/Q3BXsrrnJMBiNxhpkR4YrDjCAuCJQhksI2fE4chaAJFvJaPw5UlpCtQuiVbbqc3yXAqMc7ZCzCRvX+0srUpCaftCCjjva+MKRJRQzBKSunnKeBkSDwczfvMFeCOMUAgJNzevKG/9iDc4NV+n56zO45uDz83yZF37h0INKpfAtZ1zhvULILedD6BLrh+7SiN9L/heBGFZY/MeDx08C7N1dnUipMqmyHZYt3OLuS/tMCidJlWOzC47ZbSiX3wpWhz5pYzuaeAAOqt+/Rvbsm8RPpwnCrilLLtPJxBszSf/L+y/EDDgy6BfAaMq+4xMSOts+1hnxNL2wkja2D5aDr94d7rWSrcLEvswpKdPekGRmAJ9ph5eo4Eof0NKgZqb7BZ8WjzkDxcRvsq0aPFYPzt7XrE0PcKuY2DIHRC6dJvlmMk3QK3n9jjwd6jRN1F9n0ggIjvf5dh9uRrJ3uPhP6jwta+yGXyYZCnlkS59jZZLNJ9M8WPtctV6p2PKoZWKVLpONniR9BG3kjjVnL+SSlDSKZT1fkyu0MUzSNUiiu6H/vtZjJ4=
part2 :: [Int] -> Int
part2 = product . map ((dp !!) . length) . filter ((== 1) . head) . group . diffs
  where
    -- dp[i] represents the number of arrangements which end at adapter i.
    dp = 1 : 1 : 2 : zipWith3 (\x y z -> x + y + z) dp (drop 1 dp) (drop 2 dp)

main :: IO ()
main = do
  input <- map read . lines <$> readFile "input.txt"
  let pinput = makeList input
  print $ part1 pinput
  print $ part2 pinput

ex1 :: [Int]
ex1 =
  [ 28,
    33,
    18,
    42,
    31,
    14,
    46,
    20,
    48,
    47,
    24,
    23,
    49,
    45,
    19,
    38,
    39,
    11,
    1,
    32,
    25,
    35,
    8,
    17,
    7,
    9,
    4,
    2,
    34,
    10,
    3
  ]
