{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Writing 16 tuples of 'Truth' and 'Lies' by hand gets old after about
--   one instruction, so here is an assembler, in nandgame's dialect.
--
-- The assembler is software, not hardware, so it's allowed to use GHC's
-- type level naturals and type families. The processor itself still has to
-- do all its sums with nand gates, as is tradition.
module Assembler where

import GHC.TypeLits (Nat, Symbol, type (+), type (^), Div, Mod, CmpNat, TypeError, ErrorMessage(..))
import LogicGates
import Bytewise
import Arithmetics
import RandomAccessMemory
import ControlUnit

-- | Where a computation's result goes. Any combination works, including none,
--   which is how you compute something purely for the jump.
data Target = ToA | ToD | ToAtA

-- | The y operand: the A register, or the RAM byte A points at, written *A.
data Source = A | AtA

-- | Everything the ALU can be talked into. x is always D, y is the 'Source'.
data Expression
  = Zero
  | One
  | MinusOne
  | D
  | Y Source
  | NotD
  | NotY Source
  | NegateD
  | NegateY Source
  | DPlusOne
  | YPlusOne Source
  | DMinusOne
  | YMinusOne Source
  | DPlus Source
  | DMinus Source
  | YMinusD Source
  | DAnd Source
  | DOr Source
  | DXor Source

-- | Jump to A when the result is (j)greater than, equal to, ... zero.
--   Comparisons are signed, so 128 and up count as negative.
data Jump = Never | JGT | JEQ | JGE | JLT | JNE | JLE | JMP

data Statement
  = Label Symbol -- ^ names the address of the next instruction, takes no space in ROM
  | Literal Nat -- ^ A = n, as long as n fits in a byte
  | AddressOf Symbol -- ^ A = the address of a label, for jumping there
  | Assign [Target] Expression Jump -- ^ targets = expression ; jump
  | Halt
    -- ^ A = here + 1, then jump there, onto itself, forever. The processor takes the hint and stops.
    -- Two words, because a jump goes wherever A points and pointing A somewhere is an instruction of its own.

-- | Turn a program into ROM contents, starting at address zero.
class Assemble (program :: [Statement]) (rom :: MemoryTree InstructionWord) | program -> rom
instance Burn ZeroByte (MachineCode program) 'Untouched rom => Assemble program rom

-- | Burning a ROM uses the same 'Store' as the RAM, and counts the address up
--   with the processor's own 'Increment'. Waste not, want not.
class Burn (address :: Byte) (code :: [InstructionWord]) (rom :: MemoryTree InstructionWord) (rom' :: MemoryTree InstructionWord)
  | address code rom -> rom'
instance Burn address '[] rom rom
instance
  ( AddressBits address bits
  , Store bits word rom stored
  , Increment address next
  , Burn next rest stored rom'
  ) => Burn address (word ': rest) rom rom'

type family MachineCode (program :: [Statement]) :: [InstructionWord] where
  MachineCode program = Encode (FitsInRom (Size program)) (LabelsOf 0 program) 0 program

-- | How many ROM words a statement takes.
type family Width (statement :: Statement) :: Nat where
  Width ('Label name) = 0
  Width 'Halt = 2
  Width statement = 1

type family Size (program :: [Statement]) :: Nat where
  Size '[] = 0
  Size (statement ': rest) = Width statement + Size rest

-- | The program counter is a byte too, so 256 instructions is all you get.
--   Past that, the address would wrap and the program would quietly eat its
--   own beginning, so instead it doesn't compile.
type family FitsInRom (instructions :: Nat) :: () where
  FitsInRom instructions = FitsInRomCheck (CmpNat instructions 257) instructions

type family FitsInRomCheck (ordering :: Ordering) (instructions :: Nat) :: () where
  FitsInRomCheck 'LT instructions = '()
  FitsInRomCheck ordering instructions = TypeError
    ('Text "This program has " ':<>: 'ShowType instructions
     ':<>: 'Text " instructions, the ROM holds 256. Have you considered writing less?")

-- | Where every label points: the number of ROM words before it.
type family LabelsOf (address :: Nat) (program :: [Statement]) :: [(Symbol, Nat)] where
  LabelsOf address '[] = '[]
  LabelsOf address ('Label name ': rest) = '(name, address) ': LabelsOf address rest
  LabelsOf address (statement ': rest) = LabelsOf (address + Width statement) rest

-- | Matching on '() forces 'FitsInRom' to make up its mind, a type error
--   that never gets evaluated is just a very quiet comment.
type family Encode (fits :: ()) (labels :: [(Symbol, Nat)]) (address :: Nat) (program :: [Statement]) :: [InstructionWord] where
  Encode '() labels address '[] = '[]
  Encode '() labels address ('Label name ': rest) = Encode '() labels address rest
  Encode '() labels address ('Literal value ': rest) =
    DataWord (ByteOf value) ': Encode '() labels (address + 1) rest
  Encode '() labels address ('AddressOf name ': rest) =
    DataWord (ByteOf (Resolve name labels labels)) ': Encode '() labels (address + 1) rest
  Encode '() labels address ('Assign targets expression jump ': rest) =
    ComputeWord (Knobs expression) (Destinations targets) (JumpBits jump) ': Encode '() labels (address + 1) rest
  Encode '() labels address ('Halt ': rest) =
    DataWord (ByteOf (address + 1))
    ': ComputeWord (Knobs 'Zero) (Destinations '[]) (JumpBits 'JMP)
    ': Encode '() labels (address + 2) rest

-- | Find a label, or complain loudly. Defining a label twice is also a
--   complaint, otherwise which one you jump to is a coin toss.
type family Resolve (name :: Symbol) (labels :: [(Symbol, Nat)]) (all :: [(Symbol, Nat)]) :: Nat where
  Resolve name '[] all = TypeError ('Text "Jumping to label " ':<>: 'ShowType name ':<>: 'Text ", which doesn't exist.")
  Resolve name ('(name, address) ': rest) all = Unique name (Count name all) address
  Resolve name (label ': rest) all = Resolve name rest all

type family Count (name :: Symbol) (labels :: [(Symbol, Nat)]) :: Nat where
  Count name '[] = 0
  Count name ('(name, address) ': rest) = 1 + Count name rest
  Count name (label ': rest) = Count name rest

type family Unique (name :: Symbol) (count :: Nat) (address :: Nat) :: Nat where
  Unique name 1 address = address
  Unique name count address = TypeError
    ('Text "Label " ':<>: 'ShowType name ':<>: 'Text " is defined " ':<>: 'ShowType count ':<>: 'Text " times, pick one.")

-- | Data instruction: ci off, low byte goes to A.
type family DataWord (value :: Byte) :: InstructionWord where
  DataWord '(b7, b6, b5, b4, b3, b2, b1, b0) = '(F, F, F, F, F, F, F, F, b7, b6, b5, b4, b3, b2, b1, b0)

-- | Computation instruction, see 'InstructionWord' for which bit is which.
type family ComputeWord
    (knobs :: (Axiom, Axiom, Axiom, Axiom, Axiom, Axiom))
    (destinations :: (Axiom, Axiom, Axiom))
    (jump :: (Axiom, Axiom, Axiom)) :: InstructionWord where
  ComputeWord '(star, u, op1, op0, zx, sw) '(a, d, atA) '(lt, eq, gt) =
    '(T, F, F, star, F, u, op1, op0, zx, sw, a, d, atA, lt, eq, gt)

-- | The ALU knobs per expression: * u op1 op0 zx sw.
--   Remember sw swaps first and zx zeroes the new x, so D on its own is
--   "swap D into y, zero x, add", the scenic route.
type family Knobs (expression :: Expression) :: (Axiom, Axiom, Axiom, Axiom, Axiom, Axiom) where
  Knobs 'Zero = '(F, F, F, F, T, F)
  Knobs 'One = '(F, T, F, T, T, F)
  Knobs 'MinusOne = '(F, T, T, T, T, F)
  Knobs 'D = '(F, T, F, F, T, T)
  Knobs ('Y source) = '(Star source, T, F, F, T, F)
  Knobs 'NotD = '(F, F, T, T, F, F)
  Knobs ('NotY source) = '(Star source, F, T, T, F, T)
  Knobs 'NegateD = '(F, T, T, F, T, T)
  Knobs ('NegateY source) = '(Star source, T, T, F, T, F)
  Knobs 'DPlusOne = '(F, T, F, T, F, F)
  Knobs ('YPlusOne source) = '(Star source, T, F, T, F, T)
  Knobs 'DMinusOne = '(F, T, T, T, F, F)
  Knobs ('YMinusOne source) = '(Star source, T, T, T, F, T)
  Knobs ('DPlus source) = '(Star source, T, F, F, F, F)
  Knobs ('DMinus source) = '(Star source, T, T, F, F, F)
  Knobs ('YMinusD source) = '(Star source, T, T, F, F, T)
  Knobs ('DAnd source) = '(Star source, F, F, F, F, F)
  Knobs ('DOr source) = '(Star source, F, F, T, F, F)
  Knobs ('DXor source) = '(Star source, F, T, F, F, F)

type family Star (source :: Source) :: Axiom where
  Star 'A = F
  Star 'AtA = T

type family Destinations (targets :: [Target]) :: (Axiom, Axiom, Axiom) where
  Destinations targets = '(Mentions 'ToA targets, Mentions 'ToD targets, Mentions 'ToAtA targets)

type family Mentions (target :: Target) (targets :: [Target]) :: Axiom where
  Mentions target '[] = F
  Mentions target (target ': rest) = T
  Mentions target (other ': rest) = Mentions target rest

-- | lt eq gt
type family JumpBits (jump :: Jump) :: (Axiom, Axiom, Axiom) where
  JumpBits 'Never = '(F, F, F)
  JumpBits 'JGT = '(F, F, T)
  JumpBits 'JEQ = '(F, T, F)
  JumpBits 'JGE = '(F, T, T)
  JumpBits 'JLT = '(T, F, F)
  JumpBits 'JNE = '(T, F, T)
  JumpBits 'JLE = '(T, T, F)
  JumpBits 'JMP = '(T, T, T)

-- | A natural number as a byte, if it fits. It's an 8 bit processor, sir.
type family ByteOf (value :: Nat) :: Byte where
  ByteOf value = ByteOfChecked (CmpNat value 256) value

type family ByteOfChecked (ordering :: Ordering) (value :: Nat) :: Byte where
  ByteOfChecked 'LT value =
    '( BitOf value 7, BitOf value 6, BitOf value 5, BitOf value 4
     , BitOf value 3, BitOf value 2, BitOf value 1, BitOf value 0 )
  ByteOfChecked ordering value = TypeError
    ('Text "The literal " ':<>: 'ShowType value ':<>: 'Text " doesn't fit in a byte. It's an 8 bit processor, sir.")

type family BitOf (value :: Nat) (position :: Nat) :: Axiom where
  BitOf value position = AxiomOf (Mod (Div value (2 ^ position)) 2)

type family AxiomOf (bit :: Nat) :: Axiom where
  AxiomOf 0 = F
  AxiomOf 1 = T
