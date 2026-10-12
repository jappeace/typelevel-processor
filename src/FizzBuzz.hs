{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoOverloadedStrings #-}
-- every clock cycle nests the constraint solver one level deeper,
-- the program halting is what keeps this finite
{-# OPTIONS_GHC -freduction-depth=0 #-}

-- | The program this whole processor was built for. Interviewers hate him.
module FizzBuzz
  ( main
  , fizzBuzzLines
  , FizzBuzzProgram
  , Say(..)
  , UnknownCode(..)
  , listen
  , sayLine
  )
where

import GHC.TypeLits (Nat, type (+))
import Data.Word (Word8)
import Processor

-- | Prints what the type checker worked out while compiling this module.
--   By the time you run it, the processor has long since clocked off.
main :: IO ()
main = case fizzBuzzLines of
  Left unknown -> error ("the fizzbuzz program left an unknown code in RAM: " <> show unknown)
  Right sayings -> mapM_ putStrLn sayings

-- | Fizzbuzz from 1 to 15, the smallest range with every possible answer in it.
--   Higher is possible, but every number costs about 20 clock cycles, and every
--   clock cycle costs the type checker a tenth of a second and some 4 MB.
fizzBuzzLines :: Either UnknownCode [String]
fizzBuzzLines = fmap (fmap sayLine) (listen 15 (execute @(FizzBuzzProgram 15)))

-- | Counts from 1 to n, and for every i writes what to say into RAM at i:
--   0 for the number itself, 1 for fizz, 2 for buzz, 3 for both.
--   Buzz adds 2 on top of fizz's 1, so fizzbuzz needs no special case,
--   which is the one thing every interviewer is secretly checking for.
--
-- No division circuit, so no modulo either. Instead two countdowns, one from
-- three and one from five, and whoever hits zero gets to shout and reset.
-- RAM starts zeroed, so every number starts out as "just say the number".
--
-- n has to stay below 250, above that the numbers would trample the counters.
type FizzBuzzProgram (n :: Nat) =
  '[ 'Literal 1
   , 'Assign '[ 'ToD] ('Y 'A) 'Never
   , 'Literal I
   , 'Assign '[ 'ToAtA] 'D 'Never
   , 'Literal 3
   , 'Assign '[ 'ToD] ('Y 'A) 'Never
   , 'Literal Threes
   , 'Assign '[ 'ToAtA] 'D 'Never
   , 'Literal 5
   , 'Assign '[ 'ToD] ('Y 'A) 'Never
   , 'Literal Fives
   , 'Assign '[ 'ToAtA] 'D 'Never

   , 'Label "loop"
   , 'Literal Threes
   , 'Assign '[ 'ToD, 'ToAtA] ('YMinusOne 'AtA) 'Never
   , 'AddressOf "no fizz"
   , 'Assign '[] 'D 'JNE
   , 'Literal 3
   , 'Assign '[ 'ToD] ('Y 'A) 'Never
   , 'Literal Threes
   , 'Assign '[ 'ToAtA] 'D 'Never
   , 'Literal I
   , 'Assign '[ 'ToA] ('Y 'AtA) 'Never
   , 'Assign '[ 'ToAtA] ('YPlusOne 'AtA) 'Never

   , 'Label "no fizz"
   , 'Literal Fives
   , 'Assign '[ 'ToD, 'ToAtA] ('YMinusOne 'AtA) 'Never
   , 'AddressOf "no buzz"
   , 'Assign '[] 'D 'JNE
   , 'Literal 5
   , 'Assign '[ 'ToD] ('Y 'A) 'Never
   , 'Literal Fives
   , 'Assign '[ 'ToAtA] 'D 'Never
   , 'Literal 2
   , 'Assign '[ 'ToD] ('Y 'A) 'Never
   , 'Literal I
   , 'Assign '[ 'ToA] ('Y 'AtA) 'Never
   , 'Assign '[ 'ToAtA] ('DPlus 'AtA) 'Never

   , 'Label "no buzz"
   , 'Literal I
   , 'Assign '[ 'ToD, 'ToAtA] ('YPlusOne 'AtA) 'Never
   , 'Literal (n + 1)
   , 'Assign '[ 'ToD] ('DMinus 'A) 'Never
   , 'AddressOf "loop"
   , 'Assign '[] 'D 'JNE
   , 'Halt
   ]

-- | Where the program keeps i, and the countdowns to the next fizz and buzz.
type I = 250
type Threes = 251
type Fives = 252

-- | What the program wants said about a number.
data Say = SayNumber Word8 | SayFizz | SayBuzz | SayFizzBuzz
  deriving stock (Show, Eq)

-- | RAM held a code the program never writes, so either the program or the
--   processor is broken, and guessing which would be rude.
data UnknownCode = UnknownCode
  { unknownAt :: Word8
  , unknownCode :: Word8
  }
  deriving stock (Show, Eq)

-- | Read what the program said about 1 up to n.
listen :: Word8 -> Snapshot -> Either UnknownCode [Say]
listen n snapshot = traverse (decode snapshot) [1 .. n]

decode :: Snapshot -> Word8 -> Either UnknownCode Say
decode snapshot number = case readRam snapshot number of
  0 -> Right (SayNumber number)
  1 -> Right SayFizz
  2 -> Right SayBuzz
  3 -> Right SayFizzBuzz
  code -> Left (UnknownCode { unknownAt = number, unknownCode = code })

sayLine :: Say -> String
sayLine = \case
  SayNumber number -> show number
  SayFizz -> "Fizz"
  SayBuzz -> "Buzz"
  SayFizzBuzz -> "Fizz Buzz"
