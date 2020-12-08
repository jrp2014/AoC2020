module Day08 where

import Data.Array ( (!), (//), listArray, Array )
import Data.Either ( rights )
import Text.Parsec ( char, digit, string, choice, many1, parse )
import Text.Parsec.String ( Parser )
import Data.Maybe ( fromJust )

--https://github.com/lboshuizen/AoC-2020/blob/master/src/Day8/HandheldHalting.hs

data OpCode
  = NOP
  | ACC
  | JMP
  | HLT
  | TRP
  deriving (Eq, Show)

type Instruction = (OpCode, Int)

type Program = Array Int Instruction

type Cpu = (Int, Int) -- program counter, accumulator

opcode :: Parser OpCode
opcode = choice [NOP <$ string "nop", ACC <$ string "acc", JMP <$ string "jmp"]

instruction :: Parser Instruction
instruction = (,) <$> opcode <* string " " <*> integer

integer :: Parser Int
integer = rd <$> choice [plus, minus, number]
  where
    rd = read :: String -> Int
    plus = char '+' *> number
    minus = (:) <$> char '-' <*> number
    number = many1 digit

halt :: Instruction
halt = (HLT, 0)

trap :: Instruction
trap = (TRP, 0)

-- | tuen the instruction stream into an Array and append HLT
compile :: [Instruction] -> Program
compile instructions = prog // [(l, halt)]
  where
    l = length instructions
    prog = listArray (0, l) instructions

step :: Instruction -> Cpu -> Cpu
step (NOP, _) (pc, acc) = (pc + 1, acc)
step (ACC, n) (pc, acc) = (pc + 1, acc + n)
step (JMP, n) (pc, acc) = (pc + n, acc)
step (TRP, _) _ = error "hit trap"
step (HLT, _) _ = error "end of program"

-- | Run the program, keeping a program counter history (for debugging).
-- replacing the executed instruction with a TRP.  (This is an alternative to
-- keeping track of all the instructions executed so far as a separate set.
runTillTrap :: Cpu -> [Int] -> Program -> (Int, [Int])
runTillTrap cpu@(pc, acc) hist m
  | ins == trap = (acc, hist)
  | otherwise = runTillTrap (step ins cpu) (pc : hist) (m // [(pc, trap)])
  where
    ins = m ! pc

swap :: Instruction -> Instruction
swap (JMP, n) = (NOP, n)
swap (NOP, n) = (JMP, n)
swap x = x

red :: [Maybe a] -> Maybe a
red [] = Nothing
red (Just a : _) = Just a
red (_ : xs) = red xs

run :: Program -> (Cpu, Bool) -> Maybe Int
run prog (cpu@(pc, a), modified)
  | ins == trap = Nothing
  | ins == halt = Just a
  | otherwise = red . map (run (prog // [(pc, trap)])) $ next
  where
    ins = prog ! pc
    next =
      if modified
        then [(step ins cpu, True)]
        else [(step ins cpu, False), (step (swap ins) cpu, True)]

main :: IO ()
main = do
  input <- readFile "input.txt"
  let pinput = rights $ parse instruction "" <$> lines input
  let prog = compile pinput
  print $ fst $ runTillTrap (0, 0) [] prog
  print $ fromJust $ run prog ((0, 0), False)
