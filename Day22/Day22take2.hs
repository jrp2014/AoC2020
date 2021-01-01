module Day22take2 where

import Data.Foldable (toList)
import Data.List.Split (splitOn)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

-- https://notes.abhinavsarkar.net/2020/aoc-wk4#day-22

type Deck = Seq Int

type Hands = Set (Deck, Deck)

playCombat :: Deck -> Deck -> Deck
playCombat Empty p2 = p2
playCombat p1 Empty = p1
playCombat (x :<| xs) (y :<| ys) =
  if x > y
    then playCombat (xs :|> x :|> y) ys
    else playCombat xs (ys :|> y :|> x)

playRecCombat :: Hands -> Deck -> Deck -> Either Deck Deck
playRecCombat _ Empty p2 = Right p2
playRecCombat _ p1 Empty = Left p1
playRecCombat seen p1@(x :<| xs) p2@(y :<| ys)
  -- If there was a previous round in this game that had exactly the same cards in the same players' decks,
  -- the game instantly ends in a win for player 1
  | Set.member (p1, p2) seen = Left p1
  -- If both players have at least as many cards remaining in their deck as the value of the card they just drew,
  -- the winner of the round is determined by playing a new game
  | x <= Seq.length xs && y <= Seq.length ys =
    case playRecCombat Set.empty (Seq.take x xs) (Seq.take y ys) of
      Left _ -> playRecCombat seen' (xs :|> x :|> y) ys
      Right _ -> playRecCombat seen' xs (ys :|> y :|> x)
  -- The winner of the round is the player with the higher-value card
  -- The winner takes the two cards dealt at the beginning of the round and places them on the bottom of their
  -- own deck (so that the winner's card is above the other card), and play continues
  | x > y = playRecCombat seen' (xs :|> x :|> y) ys
  | otherwise = playRecCombat seen' xs (ys :|> y :|> x)
  where
    seen' = Set.insert (p1, p2) seen

parse :: String -> [Deck]
parse = map (Seq.fromList . map read . tail . lines) . splitOn "\n\n"

score :: Deck -> Int
score deck = sum $ zipWith (*) (toList $ Seq.reverse deck) [1 ..]

main :: IO ()
main = do
  -- let input = ex1
  input <- readFile "input.txt"
  let [player1, player2] = parse input
  print player1
  print player2
  print $ score $ playCombat player1 player2
  print $ score . either id id $ playRecCombat Set.empty player1 player2

ex1 :: String
ex1 =
  "Player 1:\n\
  \9\n\
  \2\n\
  \6\n\
  \3\n\
  \1\n\
  \\n\
  \Player 2:\n\
  \5\n\
  \8\n\
  \4\n\
  \7\n\
  \10"
