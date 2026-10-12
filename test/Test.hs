{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoOverloadedStrings #-}
-- the programs below run while this module compiles, one nesting level per clock cycle
{-# OPTIONS_GHC -freduction-depth=0 #-}

module Main where

import Data.Word (Word8)
import GHC.TypeLits (Nat, Symbol)
import Test.Tasty
import Test.Tasty.HUnit

import FizzBuzz (fizzBuzzLines)
import NatFizzBuzz (natFizzBuzz)
import Processor

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "typelevel processor"
  [ testCase "fizzbuzz on the processor agrees with fizzbuzz on GHC's arithmetic" $
      fmap (concatMap (<> ",")) fizzBuzzLines @?= Right natFizzBuzz
  , testCase "2 + 40 lands in D, and halting takes the two words of 'Halt" $ do
      let snapshot = execute @FortyTwo
      registerD snapshot @?= 42
      cycles snapshot @?= 6
      programCounter snapshot @?= 5
  , testCase "6 * 7 by adding 6 seven times in RAM" $ do
      let snapshot = execute @Multiply
      readRam snapshot 0 @?= 42
      readRam snapshot 1 @?= 0
  , testCase "every jump condition, on a negative D" $
      fmap (readRam (execute @(JumpProgram 200))) jumpSlots @?= [0, 0, 0, 0, 1, 1, 1, 1]
  , testCase "every jump condition, on a zero D" $
      fmap (readRam (execute @(JumpProgram 0))) jumpSlots @?= [0, 0, 1, 1, 0, 0, 1, 1]
  , testCase "every jump condition, on a positive D" $
      fmap (readRam (execute @(JumpProgram 5))) jumpSlots @?= [0, 1, 0, 1, 0, 1, 0, 1]
  , testCase "every expression the assembler knows, with D = 12 and *A = 10" $
      fmap (readRam (execute @AluProgram)) [1 .. 19] @?=
        [ 0, 1, 255, 12, 10
        , 243, 245, 244, 246
        , 13, 11, 11, 9
        , 22, 2, 254
        , 8, 14, 6
        ]
  ]

type FortyTwo =
  '[ 'Literal 2
   , 'Assign '[ 'ToD] ('Y 'A) 'Never
   , 'Literal 40
   , 'Assign '[ 'ToD] ('DPlus 'A) 'Never
   , 'Halt
   ]

-- RAM[1] counts down from 7, every round adds 6 to RAM[0]
type Multiply =
  '[ 'Literal 7
   , 'Assign '[ 'ToD] ('Y 'A) 'Never
   , 'Literal 1
   , 'Assign '[ 'ToAtA] 'D 'Never
   , 'Label "loop"
   , 'Literal 6
   , 'Assign '[ 'ToD] ('Y 'A) 'Never
   , 'Literal 0
   , 'Assign '[ 'ToAtA] ('DPlus 'AtA) 'Never
   , 'Literal 1
   , 'Assign '[ 'ToD, 'ToAtA] ('YMinusOne 'AtA) 'Never
   , 'AddressOf "loop"
   , 'Assign '[] 'D 'JGT
   , 'Halt
   ]

-- | RAM addresses the jump probes write to, in the order of 'Jump'.
jumpSlots :: [Word8]
jumpSlots = [0 .. 7]

-- | Put value in D, then try every jump on it. A jump that's taken writes 1
--   into its slot, one that isn't leaves the slot at its factory zero.
type JumpProgram (value :: Nat) =
  'Literal value
  ': 'Assign '[ 'ToD] ('Y 'A) 'Never
  ': JumpProbes
       '[ '( 'Never, 0, "never taken", "never done")
        , '( 'JGT, 1, "jgt taken", "jgt done")
        , '( 'JEQ, 2, "jeq taken", "jeq done")
        , '( 'JGE, 3, "jge taken", "jge done")
        , '( 'JLT, 4, "jlt taken", "jlt done")
        , '( 'JNE, 5, "jne taken", "jne done")
        , '( 'JLE, 6, "jle taken", "jle done")
        , '( 'JMP, 7, "jmp taken", "jmp done")
        ]

type family JumpProbes (probes :: [(Jump, Nat, Symbol, Symbol)]) :: [Statement] where
  JumpProbes '[] = '[ 'Halt ]
  JumpProbes ('(jump, slot, taken, done) ': rest) =
    'AddressOf taken
    ': 'Assign '[] 'D jump
    ': 'AddressOf done
    ': 'Assign '[] 'Zero 'JMP
    ': 'Label taken
    ': 'Literal slot
    ': 'Assign '[ 'ToAtA] 'One 'Never
    ': 'Label done
    ': JumpProbes rest

-- | Put 10 in RAM[100], then for every expression: D = 12, A = 100,
--   D = the expression, store D in its slot.
type AluProgram =
  'Literal 10
  ': 'Assign '[ 'ToD] ('Y 'A) 'Never
  ': 'Literal 100
  ': 'Assign '[ 'ToAtA] 'D 'Never
  ': AluProbes
       '[ '( 'Zero, 1), '( 'One, 2), '( 'MinusOne, 3), '( 'D, 4), '( 'Y 'AtA, 5)
        , '( 'NotD, 6), '( 'NotY 'AtA, 7), '( 'NegateD, 8), '( 'NegateY 'AtA, 9)
        , '( 'DPlusOne, 10), '( 'YPlusOne 'AtA, 11), '( 'DMinusOne, 12), '( 'YMinusOne 'AtA, 13)
        , '( 'DPlus 'AtA, 14), '( 'DMinus 'AtA, 15), '( 'YMinusD 'AtA, 16)
        , '( 'DAnd 'AtA, 17), '( 'DOr 'AtA, 18), '( 'DXor 'AtA, 19)
        ]

type family AluProbes (probes :: [(Expression, Nat)]) :: [Statement] where
  AluProbes '[] = '[ 'Halt ]
  AluProbes ('(expression, slot) ': rest) =
    'Literal 12
    ': 'Assign '[ 'ToD] ('Y 'A) 'Never
    ': 'Literal 100
    ': 'Assign '[ 'ToD] expression 'Never
    ': 'Literal slot
    ': 'Assign '[ 'ToAtA] 'D 'Never
    ': AluProbes rest
