{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | The part of the processor that does the actual thinking.
--   Wiring follows nandgame's ALU levels, op codes included,
--   so you can cheat off https://nandgame.com like I did.
module ArithmeticLogicUnit where

import LogicGates
import Bytewise
import Arithmetics
import Plumbing

-- | op1 op0:
--
--   * 00: x and y
--   * 01: x or y
--   * 10: x xor y
--   * 11: invert x (y gets the afternoon off)
--
-- All four get computed every time and a selector tree throws three away,
-- just like real hardware, which is equally wasteful but faster at it.
class LogicUnit (op1 :: Axiom) (op0 :: Axiom) (x :: Byte) (y :: Byte) (out :: Byte)
  | op1 op0 x y -> out
instance
  ( ByteAnd x y anded
  , ByteOr x y ored
  , ByteXor x y xored
  , Invert x inverted
  , Select8 op0 ored anded op1Off
  , Select8 op0 inverted xored op1On
  , Select8 op1 op1On op1Off out
  ) => LogicUnit op1 op0 x y out

-- | op1 op0:
--
--   * 00: x + y
--   * 01: x + 1
--   * 10: x - y
--   * 11: x - 1
--
-- See Note [One adder to rule them all].
class ArithmeticUnit (op1 :: Axiom) (op0 :: Axiom) (x :: Byte) (y :: Byte) (out :: Byte)
  | op1 op0 x y -> out
instance
  ( Select8 op0 ZeroByte y yOrNothing
  , ByteXor yOrNothing '(op1, op1, op1, op1, op1, op1, op1, op1) operand
  , Xor op1 op0 carry
  , Adder8 x operand carry out _overflow
  ) => ArithmeticUnit op1 op0 x y out

-- Note [One adder to rule them all]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The obvious arithmetic unit builds four circuits (add, increment, subtract,
-- decrement) and picks one. Every circuit costs the type checker, so instead
-- all four are one adder computing x + operand + carry:
--
--   op1 op0 | operand | carry | result
--   0   0   | y       | 0     | x + y
--   0   1   | 0       | 1     | x + 1
--   1   0   | not y   | 1     | x + not y + 1 = x - y
--   1   1   | not 0   | 0     | x + 255       = x - 1
--
-- op0 blanks y, op1 inverts whatever survived, and the carry is op1 xor op0.
-- Real ALUs pull the same trick, so this isn't even cheating.

-- | The arithmetic logic unit. Before the operation:
--
--   * sw swaps x and y,
--   * zx then replaces the (possibly swapped) x by zero.
--
-- u picks the arithmetic unit (on) or the logic unit (off).
-- Together these give you constants, negation and y - x without extra wires,
-- see "Assembler" for the full menu.
class Alu (u :: Axiom) (op1 :: Axiom) (op0 :: Axiom) (zx :: Axiom) (sw :: Axiom)
          (x :: Byte) (y :: Byte) (out :: Byte)
  | u op1 op0 zx sw x y -> out
instance
  ( Select8 sw y x left
  , Select8 sw x y right
  , Select8 zx ZeroByte left zeroedLeft
  , ArithmeticUnit op1 op0 zeroedLeft right arithmetic
  , LogicUnit op1 op0 zeroedLeft right logic
  , Select8 u arithmetic logic out
  ) => Alu u op1 op0 zx sw x y out

-- | Is x lower than (lt), equal to (eq) or greater than (gt) zero?
--   Any combination of flags works, all three means "yes, always", none means
--   "no, never", which is how jumps get to be unconditional.
class Condition (lt :: Axiom) (eq :: Axiom) (gt :: Axiom) (x :: Byte) (out :: Axiom)
  | lt eq gt x -> out
instance
  ( IsNegative x negative
  , IsZero x zero
  , Or negative zero notPositive
  , Not notPositive positive
  , And lt negative ltHolds
  , And eq zero eqHolds
  , And gt positive gtHolds
  , Or ltHolds eqHolds lowOrSame
  , Or lowOrSame gtHolds out
  ) => Condition lt eq gt x out

-- logic unit, 12 = 00001100 and 10 = 00001010
logicAndProof :: LogicUnit F F '(F, F, F, F, T, T, F, F) '(F, F, F, F, T, F, T, F) '(F, F, F, F, T, F, F, F) => b
logicAndProof = nil
logicOrProof :: LogicUnit F T '(F, F, F, F, T, T, F, F) '(F, F, F, F, T, F, T, F) '(F, F, F, F, T, T, T, F) => b
logicOrProof = nil
logicXorProof :: LogicUnit T F '(F, F, F, F, T, T, F, F) '(F, F, F, F, T, F, T, F) '(F, F, F, F, F, T, T, F) => b
logicXorProof = nil
logicInvertProof :: LogicUnit T T '(F, F, F, F, T, T, F, F) '(F, F, F, F, T, F, T, F) '(T, T, T, T, F, F, T, T) => b
logicInvertProof = nil

-- arithmetic unit, 12 and 10 again because they've been so cooperative
arithmeticAddProof :: ArithmeticUnit F F '(F, F, F, F, T, T, F, F) '(F, F, F, F, T, F, T, F) '(F, F, F, T, F, T, T, F) => b
arithmeticAddProof = nil
arithmeticIncrementProof :: ArithmeticUnit F T '(F, F, F, F, T, T, F, F) '(F, F, F, F, T, F, T, F) '(F, F, F, F, T, T, F, T) => b
arithmeticIncrementProof = nil
arithmeticSubtractProof :: ArithmeticUnit T F '(F, F, F, F, T, T, F, F) '(F, F, F, F, T, F, T, F) '(F, F, F, F, F, F, T, F) => b
arithmeticSubtractProof = nil
arithmeticSubtractNegativeProof :: ArithmeticUnit T F '(F, F, F, F, T, F, T, F) '(F, F, F, F, T, T, F, F) '(T, T, T, T, T, T, T, F) => b
arithmeticSubtractNegativeProof = nil
arithmeticDecrementProof :: ArithmeticUnit T T '(F, F, F, F, T, T, F, F) '(F, F, F, F, T, F, T, F) '(F, F, F, F, T, F, T, T) => b
arithmeticDecrementProof = nil

-- alu, x = 12, y = 10
-- u op1 op0 zx sw
aluAddProof :: Alu T F F F F '(F, F, F, F, T, T, F, F) '(F, F, F, F, T, F, T, F) '(F, F, F, T, F, T, T, F) => b
aluAddProof = nil
-- y - x thanks to sw
aluSwappedSubtractProof :: Alu T T F F T '(F, F, F, F, T, T, F, F) '(F, F, F, F, T, F, T, F) '(T, T, T, T, T, T, T, F) => b
aluSwappedSubtractProof = nil
-- 0 - y: negation for people without a negation circuit
aluNegateProof :: Alu T T F T F '(F, F, F, F, T, T, F, F) '(F, F, F, F, T, F, T, F) '(T, T, T, T, F, T, T, F) => b
aluNegateProof = nil
-- 0 + 1, the most expensive way of writing the number one
aluOneProof :: Alu T F T T F '(F, F, F, F, T, T, F, F) '(F, F, F, F, T, F, T, F) '(F, F, F, F, F, F, F, T) => b
aluOneProof = nil
aluOrProof :: Alu F F T F F '(F, F, F, F, T, T, F, F) '(F, F, F, F, T, F, T, F) '(F, F, F, F, T, T, T, F) => b
aluOrProof = nil
-- invert y by swapping it into x first
aluInvertYProof :: Alu F T T F T '(F, F, F, F, T, T, F, F) '(F, F, F, F, T, F, T, F) '(T, T, T, T, F, T, F, T) => b
aluInvertYProof = nil

-- condition, lt eq gt
conditionNeverProof :: Condition F F F '(F, F, F, F, F, F, F, F) F => b
conditionNeverProof = nil
conditionAlwaysProof :: Condition T T T '(T, F, F, F, F, F, F, F) T => b
conditionAlwaysProof = nil
conditionLessProof :: Condition T F F '(T, T, T, T, T, T, T, T) T => b
conditionLessProof = nil
conditionLessNotZeroProof :: Condition T F F '(F, F, F, F, F, F, F, F) F => b
conditionLessNotZeroProof = nil
conditionEqualProof :: Condition F T F '(F, F, F, F, F, F, F, F) T => b
conditionEqualProof = nil
conditionGreaterProof :: Condition F F T '(F, F, F, F, F, F, F, T) T => b
conditionGreaterProof = nil
-- 128 is negative here, two's complement doesn't care about your feelings
conditionGreaterNegativeProof :: Condition F F T '(T, F, F, F, F, F, F, F) F => b
conditionGreaterNegativeProof = nil
conditionNotEqualProof :: Condition T F T '(F, F, F, F, F, F, F, F) F => b
conditionNotEqualProof = nil

x :: Int -- inhabitation means proved
x = logicAndProof
    logicOrProof
    logicXorProof
    logicInvertProof
    arithmeticAddProof
    arithmeticIncrementProof
    arithmeticSubtractProof
    arithmeticSubtractNegativeProof
    arithmeticDecrementProof
    aluAddProof
    aluSwappedSubtractProof
    aluNegateProof
    aluOneProof
    aluOrProof
    aluInvertYProof
    conditionNeverProof
    conditionAlwaysProof
    conditionLessProof
    conditionLessNotZeroProof
    conditionEqualProof
    conditionGreaterProof
    conditionGreaterNegativeProof
    conditionNotEqualProof
