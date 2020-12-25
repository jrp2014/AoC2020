module Day25 where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import GHC.List (iterate')

transform :: Int -> [Int]
transform sn = iterate' ((`mod` 20201227) . (* sn)) 1

-- manual version of discrete log
loopSize :: Int -> Int -> Int
loopSize pk sn = fromJust . elemIndex pk $ transform sn

-- alternatively, transform + loopSize = powerMod
encryptionKey :: Int -> Int -> Int
encryptionKey sn ls = transform sn !! ls

part1 :: [Int] -> (Int, Int)
part1 [cardPubK, doorPubK] = (ek1, ek2)
  where
    cardLoopSize = loopSize cardPubK 7
    doorLoopSize = loopSize doorPubK 7
    ek1 = encryptionKey cardPubK doorLoopSize
    ek2 = encryptionKey doorPubK cardLoopSize

main :: IO ()
main = do
  [pubK1, pubK2] <- map read . lines <$> readFile "input.txt"
  print $ loopSize 5764801 7
  print $ loopSize 17807724 7
  print $ part1 [5764801, 17807724]
  print $ part1 [pubK1, pubK2]
