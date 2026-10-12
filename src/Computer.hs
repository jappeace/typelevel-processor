{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | The whole computer: program counter, A, D, RAM and a ROM full of hopes.
--   Ticking the clock is solving a constraint, running a program is
--   solving a lot of them. Your type checker is now a CPU, congratulations.
module Computer where

import Data.Kind (Constraint)
import GHC.TypeLits (Nat, type (+), type (-), CmpNat, TypeError, ErrorMessage(..))
import LogicGates
import Bytewise
import Arithmetics
import Memory
import RandomAccessMemory
import ControlUnit

-- | Everything that survives from one clock cycle to the next.
--   The ROM isn't in here because programs aren't supposed to rewrite
--   themselves, we're not animals.
data Computer = Computer
  Byte -- ^ program counter
  Byte -- ^ A
  Byte -- ^ D
  (MemoryTree Byte) -- ^ RAM

-- | Everything zero, RAM empty, like a fresh install before the updates.
type PowerOn = 'Computer ZeroByte ZeroByte ZeroByte 'Untouched

-- | Read the instruction the program counter points at.
--   Wandering into ROM nobody wrote is a type error rather than a quiet
--   stream of zeroes, see 'ProgramContinues'.
class Fetch (pc :: Byte) (rom :: MemoryTree InstructionWord) (instruction :: InstructionWord)
  | pc rom -> instruction
instance
  ( AddressBits pc bits
  , Lookup bits rom found
  , ProgramContinues found instruction
  ) => Fetch pc rom instruction

class ProgramContinues (found :: Maybe InstructionWord) (instruction :: InstructionWord)
  | found -> instruction
instance ProgramContinues ('Just instruction) instruction
instance
  ( TypeError ('Text "The program counter ran off the end of the program."
               ':$$: 'Text "End programs with a 'Halt, the processor stops when it jumps onto itself.")
  , instruction ~ NoInstruction
  ) => ProgramContinues 'Nothing instruction

-- | What you find past the end of a program: an instruction that never
--   reduces to any bits. The control unit can't decode it, so the clock cycle
--   stalls right there and the type error is all that's left to report.
--   Made up bits, zeroes say, would have the program keep running into
--   nothing until the fuel ran out.
type family NoInstruction :: InstructionWord where

-- | One clock cycle of nandgame's computer.
--
-- The control unit sees the current A, D and *A, and its result goes to
-- whichever of them the instruction picked. Every circuit below is wired to
-- the unprimed pc, a, d and ram, only the primed ones are new, so *A is
-- stored at the old A and a jump goes to the old A too: everybody reads
-- before anybody writes, no matter which constraint GHC solves first.
--
-- halted turns on when the next program counter equals the current one, a
-- program jumping onto itself is never going to do anything interesting again.
class Tick (rom :: MemoryTree InstructionWord) (computer :: Computer) (halted :: Axiom) (computer' :: Computer)
  | rom computer -> halted computer'
instance
  ( Fetch pc rom instruction
  , ControlUnit instruction a d atA result toA toD toAtA jump
  , Ram a toAtA result ram atA ram'
  , Register toA result a a'
  , Register toD result d d'
  , Counter jump a pc pc'
  , Equal pc pc' halted
  ) => Tick rom ('Computer pc a d ram) halted ('Computer pc' a' d' ram')

-- | Tick until the program halts, also counting how many cycles that took.
--
-- The next cycle only starts once halted is known, because 'Continue' can't
-- pick an instance before then. That keeps the type checker working on one
-- cycle at a time instead of unrolling the program into infinity.
--
-- fuel is how many cycles the program may take. Without it a program that
-- never halts has GHC eat all your memory in silence, which is the halting
-- problem expressed as a swap file.
class Run (rom :: MemoryTree InstructionWord) (computer :: Computer) (fuel :: Nat) (cycles :: Nat) (final :: Computer)
  | rom computer fuel -> cycles final
instance
  ( Tick rom computer halted next
  , Continue halted rom next fuel cycles final
  ) => Run rom computer fuel cycles final

class Continue (halted :: Axiom) (rom :: MemoryTree InstructionWord) (computer :: Computer) (fuel :: Nat) (cycles :: Nat) (final :: Computer)
  | halted rom computer fuel -> cycles final
instance Continue T rom computer fuel 1 computer
instance Refuel (CmpNat fuel 1) rom computer fuel cycles final => Continue F rom computer fuel cycles final

-- | Pays for the next cycle out of the fuel, or ends things with a type error.
--
-- The next 'Run' lives inside the instance rather than next to it on purpose:
-- this way it doesn't exist until the fuel check picked an instance. Put next
-- to it, the type checker would keep running the program while the type
-- error waits patiently for a finish that never comes.
class Refuel (comparison :: Ordering) (rom :: MemoryTree InstructionWord) (computer :: Computer) (fuel :: Nat) (cycles :: Nat) (final :: Computer)
  | comparison rom computer fuel -> cycles final
instance
  ( Run rom computer (fuel - 1) remaining final
  , cycles ~ (remaining + 1)
  ) => Refuel 'GT rom computer fuel cycles final
-- the right hand sides are never used, the type error gets there first
instance OutOfFuel => Refuel 'EQ rom computer fuel 0 computer
-- a limit of zero cycles, for people who like their programs unexecuted
instance OutOfFuel => Refuel 'LT rom computer fuel 0 computer

type OutOfFuel = (TypeError
  ('Text "The program is still running and the cycle limit is used up."
   ':$$: 'Text "Either it never reaches a 'Halt, or it needs a bigger limit for executeWithin.")
  :: Constraint)
