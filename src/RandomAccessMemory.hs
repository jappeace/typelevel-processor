{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | 256 bytes of RAM, which is 255 more than the type checker wanted.
module RandomAccessMemory where

import LogicGates
import Bytewise
import Memory

-- | Memory as a binary tree, one level per address bit, most significant first.
--   Off (F) goes left, on (T) goes right.
--
-- Decision: a tree that only grows the paths a program actually visits.
-- Wiring up all 256 registers with an address decoder is what the hardware
-- does, but then every clock cycle the type checker has to solve 2048
-- flip-flops that, with their store bit off, would hold their value anyway.
-- Walking down a single path gives the same answers for 1/256th of the price.
-- A flat 256-tuple was the other option, and nobody wants to read that error message.
data MemoryTree word
  = Cell word -- ^ a word someone bothered to put here
  | Branch (MemoryTree word) (MemoryTree word)
  | Untouched -- ^ nobody has been here, it reads as zero, or as nothing at all for the ROM

-- | The address wires, most significant first. Pure wiring, no gates were harmed.
class AddressBits (address :: Byte) (bits :: [Axiom]) | address -> bits
instance AddressBits '(a7, a6, a5, a4, a3, a2, a1, a0) '[a7, a6, a5, a4, a3, a2, a1, a0]

-- | Look up whatever lives at an address, 'Nothing for places nobody has been.
--   A tree that runs out of levels before the address runs out of bits has no
--   instance, so a malformed tree is a type error instead of a guess.
class Lookup (address :: [Axiom]) (tree :: MemoryTree word) (found :: Maybe word) | address tree -> found
instance Lookup '[] ('Cell word) ('Just word)
instance Lookup address 'Untouched 'Nothing
instance Lookup rest left found => Lookup (F ': rest) ('Branch left right) found
instance Lookup rest right found => Lookup (T ': rest) ('Branch left right) found

-- | Put a word at an address, building the path there if needed.
class Store (address :: [Axiom]) (word :: k) (tree :: MemoryTree k) (tree' :: MemoryTree k)
  | address word tree -> tree'
instance Store '[] word tree ('Cell word)
instance Store (bit ': rest) word ('Branch 'Untouched 'Untouched) tree'
  => Store (bit ': rest) word 'Untouched tree'
instance Store rest word left left'
  => Store (F ': rest) word ('Branch left right) ('Branch left' right)
instance Store rest word right right'
  => Store (T ': rest) word ('Branch left right) ('Branch left right')

-- | RAM wakes up with nothing in it. Literally zero.
class ZeroWhenUntouched (found :: Maybe Byte) (value :: Byte) | found -> value
instance ZeroWhenUntouched 'Nothing ZeroByte
instance ZeroWhenUntouched ('Just value) value

-- | nandgame's RAM: emits the byte at the address, and on the clock cycle it
--   stores x there when st is on.
--
-- The addressed cell is a register like any other, so storing goes through the
-- same flip-flops as A and D do. Only the addressed cell gets clocked, see the
-- Decision at 'MemoryTree'.
class Ram (address :: Byte) (st :: Axiom) (x :: Byte) (ram :: MemoryTree Byte)
          (out :: Byte) (ram' :: MemoryTree Byte)
  | address ram -> out
  , address st x ram -> ram'
instance
  ( AddressBits address bits
  , Lookup bits ram found
  , ZeroWhenUntouched found out
  , Register st x out next
  , Store bits next ram ram'
  ) => Ram address st x ram out ram'

-- 42 = 00101010, mostly because I always use 42
ramFreshProof :: Ram '(F, F, F, F, F, F, F, T) F '(F, F, T, F, T, F, T, F) 'Untouched '(F, F, F, F, F, F, F, F) ram' => b
ramFreshProof = nil

ramStoreThenReadProof ::
  ( Ram '(T, F, F, F, F, F, F, T) T '(F, F, T, F, T, F, T, F) 'Untouched before written
  , Ram '(T, F, F, F, F, F, F, T) F '(F, F, F, F, F, F, F, F) written '(F, F, T, F, T, F, T, F) ram'
  ) => b
ramStoreThenReadProof = nil

-- writing one address must leave its neighbours alone, gossip doesn't travel in RAM
ramNeighbourProof ::
  ( Ram '(T, F, F, F, F, F, F, T) T '(F, F, T, F, T, F, T, F) 'Untouched before written
  , Ram '(T, F, F, F, F, F, F, F) F '(F, F, F, F, F, F, F, F) written '(F, F, F, F, F, F, F, F) ram'
  ) => b
ramNeighbourProof = nil

-- st off means the byte on x goes nowhere
ramNoStoreProof ::
  ( Ram '(F, F, F, F, F, F, T, F) F '(F, F, T, F, T, F, T, F) 'Untouched before written
  , Ram '(F, F, F, F, F, F, T, F) F '(F, F, F, F, F, F, F, F) written '(F, F, F, F, F, F, F, F) ram'
  ) => b
ramNoStoreProof = nil

ramOverwriteProof ::
  ( Ram '(F, F, F, F, F, F, T, F) T '(F, F, T, F, T, F, T, F) 'Untouched before written
  , Ram '(F, F, F, F, F, F, T, F) T '(F, F, F, F, F, T, T, T) written '(F, F, T, F, T, F, T, F) overwritten
  , Ram '(F, F, F, F, F, F, T, F) F '(F, F, F, F, F, F, F, F) overwritten '(F, F, F, F, F, T, T, T) ram'
  ) => b
ramOverwriteProof = nil

x :: Int -- inhabitation means proved
x = ramFreshProof
    ramStoreThenReadProof
    ramNeighbourProof
    ramNoStoreProof
    ramOverwriteProof
