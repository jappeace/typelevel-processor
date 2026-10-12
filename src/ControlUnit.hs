{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Reads an instruction and bosses everybody else around.
module ControlUnit where

import LogicGates
import Bytewise
import Plumbing
import ArithmeticLogicUnit

-- | Instructions are 16 bits, most significant first, while the data is 8 bits.
--
-- Decision: 8 bit data with 16 bit instructions, like an AVR keeps its
-- instructions in a separate, wider memory. nandgame's control unit needs 13
-- control bits, so an 8 bit instruction would have meant inventing a new
-- instruction set with fewer features. That would be work, and work is what we
-- built this computer to avoid.
--
-- Bit layout, same as nandgame:
--
--   * 15: ci, on for a computation, off for "put the low byte in A"
--   * 12: * , read y from RAM at A (*A) instead of from A itself
--   * 10: u , 9: op1, 8: op0, 7: zx, 6: sw, the ALU knobs
--   * 5: a, 4: d, 3: *a, where the result goes, any combination
--   * 2: lt, 1: eq, 0: gt, jump to A if the result is lt/eq/gt zero
--
-- Bits 14, 13 and 11 do nothing, they're just there for the vibes.
type InstructionWord =
  ( Axiom, Axiom, Axiom, Axiom, Axiom, Axiom, Axiom, Axiom
  , Axiom, Axiom, Axiom, Axiom, Axiom, Axiom, Axiom, Axiom
  )

-- | Takes the instruction plus A, D and *A, works out the result and who gets it.
--   x is always D, y is A or *A.
--
-- A data instruction (ci off) is not that clever: the low byte goes to A,
-- nothing else gets touched, and there's definitely no jumping.
class ControlUnit (instruction :: InstructionWord) (a :: Byte) (d :: Byte) (atA :: Byte)
                  (result :: Byte) (toA :: Axiom) (toD :: Axiom) (toAtA :: Axiom) (jump :: Axiom)
  | instruction a d atA -> result toA toD toAtA jump
instance
  ( Select8 i12 atA a y
  , Alu i10 i9 i8 i7 i6 d y computed
  , Select8 i15 computed '(i7, i6, i5, i4, i3, i2, i1, i0) result
  , Selector i15 i5 T toA
  , And i15 i4 toD
  , And i15 i3 toAtA
  , Condition i2 i1 i0 computed condition
  , And i15 condition jump
  ) => ControlUnit
         '(i15, _i14, _i13, i12, _i11, i10, i9, i8, i7, i6, i5, i4, i3, i2, i1, i0)
         a d atA result toA toD toAtA jump

-- A = 42
controlDataProof :: ControlUnit
  '(F, F, F, F, F, F, F, F, F, F, T, F, T, F, T, F)
  '(F, F, F, F, F, F, F, T) '(F, F, F, F, F, F, T, F) '(F, F, F, F, F, F, T, T)
  '(F, F, T, F, T, F, T, F) T F F F => b
controlDataProof = nil

-- D = D + A, with A = 1 and D = 2
controlComputeProof :: ControlUnit
  '(T, F, F, F, F, T, F, F, F, F, F, T, F, F, F, F)
  '(F, F, F, F, F, F, F, T) '(F, F, F, F, F, F, T, F) '(F, F, F, F, F, F, T, T)
  '(F, F, F, F, F, F, T, T) F T F F => b
controlComputeProof = nil

-- A, D, *A = D + *A, with D = 2 and *A = 3
controlStarProof :: ControlUnit
  '(T, F, F, T, F, T, F, F, F, F, T, T, T, F, F, F)
  '(F, F, F, F, F, F, F, T) '(F, F, F, F, F, F, T, F) '(F, F, F, F, F, F, T, T)
  '(F, F, F, F, F, T, F, T) T T T F => b
controlStarProof = nil

-- D ; JGT with D = 2: jumps
controlJumpProof :: ControlUnit
  '(T, F, F, F, F, T, F, F, T, T, F, F, F, F, F, T)
  '(F, F, F, F, F, F, F, T) '(F, F, F, F, F, F, T, F) '(F, F, F, F, F, F, T, T)
  '(F, F, F, F, F, F, T, F) F F F T => b
controlJumpProof = nil

-- D ; JLT with D = 2: doesn't
controlNoJumpProof :: ControlUnit
  '(T, F, F, F, F, T, F, F, T, T, F, F, F, T, F, F)
  '(F, F, F, F, F, F, F, T) '(F, F, F, F, F, F, T, F) '(F, F, F, F, F, F, T, T)
  '(F, F, F, F, F, F, T, F) F F F F => b
controlNoJumpProof = nil

-- a data instruction whose low bits look like jump flags still doesn't jump
controlDataNeverJumpsProof :: ControlUnit
  '(F, F, F, F, F, F, F, F, F, F, F, F, F, T, T, T)
  '(F, F, F, F, F, F, F, T) '(F, F, F, F, F, F, T, F) '(F, F, F, F, F, F, T, T)
  '(F, F, F, F, F, T, T, T) T F F F => b
controlDataNeverJumpsProof = nil

x :: Int -- inhabitation means proved
x = controlDataProof
    controlComputeProof
    controlStarProof
    controlJumpProof
    controlNoJumpProof
    controlDataNeverJumpsProof
