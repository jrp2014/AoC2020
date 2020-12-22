{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Day22 where

-- https://github.com/mstksg/advent-of-code-2020/blob/master/src/AOC/Challenge/Day22.hs

import Control.Monad (guard)
import Data.Foldable (Foldable (toList))
import Data.Hashable (Hashable (..), hash)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM (alterF, empty)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
  ( Seq (Empty),
    fromList,
    length,
    take,
  )
import Data.Set (Set)
import qualified Data.Set as S (insert, notMember, singleton)
import Text.Parsec (digit, many, many1, newline, parse, string)
import Text.Parsec.String (Parser)

type Deck = Seq Int

data Player = P1 | P2
  deriving (Show, Eq, Ord)

score :: Deck -> Int
score = sum . zipWith (*) [1 ..] . reverse . toList

playGameWith ::
  -- | recurse
  (Deck -> Deck -> Maybe Player) ->
  Deck ->
  Deck ->
  (Player, Deck)
playGameWith f = go IM.empty
  where
    go :: IntMap (Set (Deck, Deck)) -> Deck -> Deck -> (Player, Deck)
    go !seen !xs0 !ys0 = case addSeen of
      Nothing -> (P1, xs0)
      Just seen' -> case (xs0, ys0) of
        (x :<| xs, y :<| ys) ->
          let winner = case f (x :<| xs) (y :<| ys) of
                Nothing -> if x > y then P1 else P2
                Just p -> p
           in case winner of
                P1 -> go seen' (xs :|> x :|> y) ys
                P2 -> go seen' xs (ys :|> y :|> x)
        (Seq.Empty, _) -> (P2, ys0)
        (_, Seq.Empty) -> (P1, xs0)
      where
        handTup = (xs0, ys0)
        addSeen =
          IM.alterF
            ( \case
                Nothing -> Just (Just (S.singleton handTup))
                Just s -> Just (S.insert handTup s) <$ guard (handTup `S.notMember` s)
            )
            (hashHand xs0 ys0)
            seen

hashHand :: Deck -> Deck -> Int
hashHand xs ys =
  hash
    ( take 2 (toList xs),
      take 2 (toList ys),
      Seq.length xs
    )

gameParser :: Parser (Deck, Deck)
gameParser = (,) <$> deckParser <*> deckParser
  where
    deckParser = do
      _ <- string "Player " *> number <* string ":" <* newline
      fmap Seq.fromList . many $ number <* many newline

    number :: Parser Int
    number = do
      digits <- many1 digit
      return (read digits)

part1 :: Deck -> Deck -> Int
part1 deck1 deck2 = score winningDeck
  where
    (_, winningDeck) = playGameWith (\_ _ -> Nothing) deck1 deck2

part2 :: Deck -> Deck -> Int
part2 deck1 deck2 = score winningDeck
  where
    (_, winningDeck) = game2 deck1 deck2

    game2 :: Deck -> Deck -> (Player, Deck)
    game2 = playGameWith $ \(x :<| xs) (y :<| ys) -> do
      xs' <- takeExactly x xs
      ys' <- takeExactly y ys
      let xMax = maximum xs'
          yMax = maximum ys'
      if xMax > yMax
        then pure P1 -- P1 has unbeatable card
        else pure . fst $ game2 xs' ys'

    takeExactly :: Int -> Seq a -> Maybe (Seq a)
    takeExactly n xs = Seq.take n xs <$ guard (Seq.length xs >= n)

main :: IO ()
main = do
  input <- readFile "input.txt"
  --  print $ parse gameParser "" input
  let Right (deck1, deck2) = parse gameParser "" input
  print $ part1 deck1 deck2
  print $ part2 deck1 deck2

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
