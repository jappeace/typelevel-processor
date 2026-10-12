{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Plumbing where

import LogicGates
import Bytewise

-- | A select-component selects one out of two input bits for output.
--
--   The s (select) bit indicates which input is selected:
--   If 0, d0 is selected, if 1, d1 is selected.
--
--   Straight from nand rather than via and/or: four gates instead of eight.
--   The processor is mostly selectors, so this is where the type checker
--   spends its evenings.
class Selector (s :: Axiom) (d1 :: Axiom) (d0 :: Axiom) (out :: Axiom) | s d0 d1 -> out

instance
  (
    Nand s s notS
  , Nand s d1 notPickD1
  , Nand notS d0 notPickD0
  , Nand notPickD1 notPickD0 out
  ) =>
  Selector  s d1 d0 out

selectorProof1 :: Selector F F F F => b
selectorProof1 = nil
selectorProof2 :: Selector F T F F => b
selectorProof2 = nil
selectorProof3 :: Selector F F T T => b
selectorProof3 = nil
selectorProof4 :: Selector F T T T => b
selectorProof4 = nil
selectorProof5 :: Selector T F F F => b
selectorProof5 = nil
selectorProof6 :: Selector T F T F => b
selectorProof6 = nil
selectorProof7 :: Selector T T F T => b
selectorProof7 = nil
selectorProof8 :: Selector T T T T => b
selectorProof8 = nil

class Switch s d c1 c0 | s d -> c1 c0

instance
  (
    And s d c1
  , Xor c1 d c0
  ) => Switch s d c1 c0
switchProof1 :: Switch F F F F => b
switchProof1 = nil
switchProof2 :: Switch F T F T => b
switchProof2 = nil
switchProof3 :: Switch T F F F => b
switchProof3 = nil
switchProof4 :: Switch T T T F => b
switchProof4 = nil

-- | A selector for a whole byte, so the processor can make up its mind
--   eight wires at a time. s picks d1 when on, d0 when off.
class Select8 (s :: Axiom) (d1 :: Byte) (d0 :: Byte) (out :: Byte) | s d1 d0 -> out
instance
  ( Selector s a7 b7 o7, Selector s a6 b6 o6, Selector s a5 b5 o5, Selector s a4 b4 o4
  , Selector s a3 b3 o3, Selector s a2 b2 o2, Selector s a1 b1 o1, Selector s a0 b0 o0
  ) => Select8 s
         '(a7, a6, a5, a4, a3, a2, a1, a0)
         '(b7, b6, b5, b4, b3, b2, b1, b0)
         '(o7, o6, o5, o4, o3, o2, o1, o0)

select8Proof1 :: Select8 T '(T, T, F, F, T, F, T, F) '(F, F, F, F, T, T, T, T) '(T, T, F, F, T, F, T, F) => b
select8Proof1 = nil
select8Proof2 :: Select8 F '(T, T, F, F, T, F, T, F) '(F, F, F, F, T, T, T, T) '(F, F, F, F, T, T, T, T) => b
select8Proof2 = nil

x :: Int -- inhabitation means proved
x =
   selectorProof1
   selectorProof2
   selectorProof3
   selectorProof4
   selectorProof5
   selectorProof6
   selectorProof7
   selectorProof8
   switchProof1
   switchProof2
   switchProof3
   switchProof4
   select8Proof1
   select8Proof2
