module Day23 where

import Data.Char (digitToInt)
import Data.Sequence (Seq (..), (<|), (><), (|>))
import qualified Data.Sequence as S

nIters :: Int
nIters = 100

nCups :: Int
nCups = 9

applyN :: Int -> (a -> a) -> a -> a
applyN n f = foldr (.) id (replicate n f)

-- to get the destination cup
dec :: Int -> Int
dec 0 = nCups - 1
dec x = x - 1

-- a turn of the game
turn :: Seq Int -> Seq Int
turn (current :<| cup2 :<| cup3 :<| cup4 :<| rest) =
  turn' current (dec current) (cup2 <| cup3 <| cup4 <| S.empty) rest
  where
    -- cup number, destination cup, picked cups, and rest of the cups
    turn' :: Int -> Int -> Seq Int -> Seq Int -> Seq Int
    turn' current destination picked rest =
      if destination `elem` picked
        then turn' current (dec destination) picked rest -- so try again, with a reduced destination
        else turn'' current destination picked rest

    -- place the cups just picked up so that they are immediately clockwise of the destination cup.
    turn'' :: Int -> Int -> Seq Int -> Seq Int -> Seq Int
    turn'' current destination picked rest =
      -- rest1 and rest2 are the subsequences to the left and right of destination
      let (rest1, _ :<| rest2) = S.spanl (/= destination) rest
       in (rest1 >< destination <| picked >< rest2) |> current -- rotate out the current cup too

part1 :: [Int] -> String
part1 xs = concatMap (show . succ) $ xs2 >< xs1
  where
    xs' :: [Int]
    xs' = map pred xs -- used 0-based cup numbers

    (xs1, _ :<| xs2) = S.spanl (/= 0) $ applyN nIters turn $ S.fromList xs'

main :: IO ()
main = do
  --let input = map digitToInt ex1
  let input = map digitToInt "925176834"
  putStrLn $ part1 input

ex1 :: String
ex1 = "389125467"
