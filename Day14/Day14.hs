module Day14 where

import           Data.Bits          (Bits (bit, complement, testBit, (.&.), (.|.)))
import           Data.Either        (fromRight)
import qualified Data.IntMap        as M (IntMap, empty, insert)
import           Data.List          (foldl', subsequences)
import           Text.Parsec        (anyChar, digit, many1, parse, string, try,
                                     (<|>))
import           Text.Parsec.String (Parser)

-- https://github.com/haskelling/aoc2020/blob/main/14a.hs

type Memory = M.IntMap Int

-- The mask tuple contains (Xs, 1s).  Could also use a tuple for the loads...
data Instr = Mask (Int, Int) | Load Int Int deriving (Show, Eq, Read, Ord)

parseLine :: Parser Instr
parseLine = try loadp <|> maskp

loadp :: Parser Instr
loadp = do
  _    <- string "mem["
  addr <- natural
  _    <- string "] = "
  Load addr <$> natural

maskp :: Parser Instr
maskp = do
  _    <- string "mask = "
  mask <- many1 anyChar
  return $ Mask $ readMask mask

natural :: Parser Int
natural = read <$> many1 digit

-- reads a list of X, 0 or 1
readMask :: String -> (Int, Int)
readMask = foldl' (\x y -> 2 *$ x + readMask' y) 0
 where
  readMask' 'X' = (1, 0)
  readMask' '0' = (0, 0)
  readMask' '1' = (0, 1)

-- scalar multiplication
(*$) :: Int -> (Int, Int) -> (Int, Int)
n *$ (x, y) = (n * x, n * y)

instance (Num a, Num b) => Num (a, b) where
  (x, y) + (u, v) = (x + u, y + v)
  (x, y) * (u, v) = (x * u, y * v)
  negate (x, y) = (negate x, negate y)
  fromInteger x = (fromInteger x, 0)
  abs (x, y) = (abs x, abs y)
  signum (x, y) = (signum x, signum y)

part1 :: [Instr] -> Int
part1 is = sum . snd $ foldl' exec (0, M.empty) is
 where
  exec :: ((Int, Int), Memory) -> Instr -> ((Int, Int), Memory)
  exec (mask, mem) (Load addr val) =
    (mask, M.insert addr (applyMask mask val) mem)
  exec (_mask, mem) (Mask x) = (x, mem)

  applyMask :: (Int, Int) -> Int -> Int
  applyMask (mask, set) val = set .|. val .&. mask

part2 :: [Instr] -> Int
part2 is = sum $ snd $ foldl' exec (0, M.empty) is
 where
  exec :: ((Int, Int), Memory) -> Instr -> ((Int, Int), Memory)
  exec (mask , mem) (Load addr val) = (mask, updateMap addr mask val mem)
  exec (_mask, mem) (Mask x       ) = (x, mem)

  updateMap :: Int -> (Int, Int) -> Int -> M.IntMap Int -> M.IntMap Int
  updateMap addr mask val mem = insertMany val (flAddrs mask addr) mem

  insertMany :: Int -> [Int] -> M.IntMap Int -> M.IntMap Int
  insertMany val addrs m = foldl' (\m' a -> M.insert a val m') m addrs

  flAddrs :: (Int, Int) -> Int -> [Int]
  flAddrs (mask, set) addr =
    map (\x -> sum x .|. set .|. addr .&. complement mask)
      $ floatingAddresses mask

  floatingAddresses :: Int -> [[Int]]
  floatingAddresses mask =
    subsequences [ bit b | b <- [0 .. 35], testBit mask b ]

main :: IO ()
main = do
  --let input = lines ex1
  input <- lines <$> readFile "input.txt"
  let pinput = fromRight (error "parse error") . parse parseLine "" <$> input
  print $ part1 pinput
  print $ part2 pinput

ex1 :: String
ex1 =
  "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n\
  \mem[8] = 11\n\
  \mem[7] = 101\n\
  \mem[8] = 0\n"

ex2 :: String
ex2 =
  "mask = 000000000000000000000000000000X1001X\n\
  \mem[42] = 100\n\
  \mask = 00000000000000000000000000000000X0XX\n\
  \mem[26] = 1\n"
