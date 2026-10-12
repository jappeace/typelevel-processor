{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Dragging results out of the type checker into the world of values,
--   where they can finally be printed and judged.
module Reflection
  ( KnownAxiom(..)
  , KnownByte(..)
  , KnownMemoryTree(..)
  , KnownComputer(..)
  , Snapshot(..)
  , readRam
  )
where

import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Numeric.Natural (Natural)
import LogicGates
import Bytewise
import RandomAccessMemory
import Computer

class KnownAxiom (axiom :: Axiom) where
  axiomVal :: Bool
instance KnownAxiom Truth where
  axiomVal = True
instance KnownAxiom Lies where
  axiomVal = False

class KnownByte (byte :: Byte) where
  byteVal :: Word8
instance
  ( KnownAxiom b7, KnownAxiom b6, KnownAxiom b5, KnownAxiom b4
  , KnownAxiom b3, KnownAxiom b2, KnownAxiom b1, KnownAxiom b0
  ) => KnownByte '(b7, b6, b5, b4, b3, b2, b1, b0) where
  byteVal = foldl shiftInBit 0
    [ axiomVal @b7, axiomVal @b6, axiomVal @b5, axiomVal @b4
    , axiomVal @b3, axiomVal @b2, axiomVal @b1, axiomVal @b0 ]

shiftInBit :: Word8 -> Bool -> Word8
shiftInBit soFar bit = soFar * 2 + (if bit then 1 else 0)

-- | Every cell the program visited, as (address, value).
class KnownMemoryTree (tree :: MemoryTree Byte) where
  -- | Takes the address bits walked so far, so the leaves know who they are.
  memoryCells :: Word8 -> [(Word8, Word8)]
instance KnownMemoryTree 'Untouched where
  memoryCells _path = []
instance KnownByte value => KnownMemoryTree ('Cell value) where
  memoryCells path = [(path, byteVal @value)]
instance (KnownMemoryTree left, KnownMemoryTree right) => KnownMemoryTree ('Branch left right) where
  memoryCells path = memoryCells @left (shiftInBit path False) <> memoryCells @right (shiftInBit path True)

-- | The computer, frozen in time, inspectable by mere mortals.
data Snapshot = Snapshot
  { cycles :: Natural -- ^ clock cycles until the program halted, halting cycle included
  , programCounter :: Word8
  , registerA :: Word8
  , registerD :: Word8
  , ramCells :: [(Word8, Word8)] -- ^ only the cells the program visited, by address
  }
  deriving stock (Show, Eq)

class KnownComputer (computer :: Computer) where
  snapshotAfter :: Natural -> Snapshot
instance (KnownByte pc, KnownByte a, KnownByte d, KnownMemoryTree ram) => KnownComputer ('Computer pc a d ram) where
  snapshotAfter elapsed = Snapshot
    { cycles = elapsed
    , programCounter = byteVal @pc
    , registerA = byteVal @a
    , registerD = byteVal @d
    , ramCells = memoryCells @ram 0
    }

-- | A RAM cell the program never visited still holds what it booted with: zero.
readRam :: Snapshot -> Word8 -> Word8
readRam snapshot address = fromMaybe 0 (lookup address (ramCells snapshot))
