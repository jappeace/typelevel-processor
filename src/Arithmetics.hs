{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | Counting, but with extra steps.
module Arithmetics where

import LogicGates
import Bytewise

-- | An add component that adds two bits. The output is a two-bit value.
-- | The h output is the high bit, the l is the low bit.

class HalfAdder (a :: Axiom) (b :: Axiom) (h :: Axiom) (l :: Axiom) | a b -> h l

instance ( And a b h
         , Xor a b l) =>
  HalfAdder a b h l

halfAdderProof1 :: HalfAdder F F F F => b
halfAdderProof1 = nil
halfAdderProof2 :: HalfAdder F T F T => b
halfAdderProof2 = nil
halfAdderProof3 :: HalfAdder T F F T => b
halfAdderProof3 = nil
halfAdderProof4 :: HalfAdder T T T F => b
halfAdderProof4 = nil

-- | An add component which adds three bits: a, b, and c.
-- The output is a two-bit value. The h output is the high bit, the l is the low bit.
--
-- The first version had seven parts and a theory about which cases were
-- special. This is the textbook one: add two bits, add the third to that,
-- and at most one of the two half adders can carry.
class Adder1 (a :: Axiom) (b :: Axiom) (c :: Axiom) (h :: Axiom) (l :: Axiom) | a b c -> h l

instance (
           HalfAdder a b carryAB sumAB
         , HalfAdder sumAB c carryC l
         , Or carryAB carryC h
         ) =>
  Adder1 a b c h l

-- Input	Output
-- a	b	c	h	l
adderProof1 :: Adder1 F F F F F=> b
adderProof1 = nil
adderProof2 :: Adder1 F F T F T=> b
adderProof2 = nil
adderProof3 :: Adder1 F T F F T => b
adderProof3 = nil
adderProof4 :: Adder1 F T T T F => b
adderProof4 = nil
adderProof5 :: Adder1 T F F F T => b
adderProof5 = nil
adderProof6 :: Adder1 T F T T F => b
adderProof6 = nil
adderProof7 :: Adder1 T T F T F => b
adderProof7 = nil
adderProof8 :: Adder1 T T T T T => b
adderProof8 = nil


-- | Build an adder that adds two 2-bit numbers and a 1-bit carry.
-- * Input
-- a1 a0 is a 2-bit number.
-- b1 b0 is a 2-bit number.
-- c (input carry) is a 1-bit number.
-- * Output
-- The sum of the input numbers as the 3-bit number c s1 s0 where c is the high bit.

class Adder2 (a :: (Axiom, Axiom)) (b :: (Axiom, Axiom)) (c :: Axiom)
                    (s :: (Axiom, Axiom)) (cout :: Axiom)
                    | a b c -> cout s

instance ( Adder1 c b0 a0 midh s0
         , Adder1 midh a1 b1 cout s1
         ) =>  Adder2 '(a1,a0) '(b1,b0) c '(s1,s0) cout

_2bitAdderProof1  :: Adder2 '(F,F) '(F,F) F '(F,F) F=> b
_2bitAdderProof1  = nil
_2bitAdderProof2  :: Adder2 '(F,F) '(F,F) T '(F,T) F=> b
_2bitAdderProof2  = nil
_2bitAdderProof3  :: Adder2 '(F,F) '(F,T) F '(F,T) F=> b
_2bitAdderProof3  = nil
_2bitAdderProof4  :: Adder2 '(F,F) '(F,T) T '(T,F) F=> b
_2bitAdderProof4  = nil
_2bitAdderProof5  :: Adder2 '(F,F) '(T,F) F '(T,F) F=> b
_2bitAdderProof5  = nil
_2bitAdderProof6  :: Adder2 '(F,F) '(T,F) T '(T,T) F=> b
_2bitAdderProof6  = nil
_2bitAdderProof7  :: Adder2 '(F,F) '(T,T) F '(T,T) F=> b
_2bitAdderProof7  = nil
_2bitAdderProof8  :: Adder2 '(F,F) '(T,T) T '(F,F) T=> b
_2bitAdderProof8  = nil
_2bitAdderProof9  :: Adder2 '(F,T) '(F,F) F '(F,T) F=> b
_2bitAdderProof9  = nil
_2bitAdderProof10 :: Adder2 '(F,T) '(F,F) T '(T,F) F=> b
_2bitAdderProof10 = nil

class Adder4 (a :: (Axiom, Axiom, Axiom, Axiom)) (b :: (Axiom, Axiom, Axiom, Axiom)) (c :: Axiom)
                   (s :: (Axiom, Axiom, Axiom, Axiom)) (cout :: Axiom)  | a b c -> cout s

instance (
    Adder2 '(a1, a0) '(b1, b0) c '(s1, s0) midh
    , Adder2 '(a3, a2) '(b3, b2) midh '(s3, s2) cout
                      ) => Adder4 '(a3, a2, a1, a0) '(b3, b2, b1, b0) c '(s3, s2, s1, s0) cout

_4bitAdderProof3  :: Adder4 '(T, T, F, F)  '(F, F, F, T) F '(T, T, F, T)  F=> b
_4bitAdderProof3  = nil
_4bitAdderProof4  :: Adder4 '(T, T, F, T)  '(F, F, F, F) T '(T, T, T, F)  F=> b
_4bitAdderProof4  = nil
_4bitAdderProof5  :: Adder4 '(T, T, F, T)  '(F, F, T, F) F '(T, T, T, T)  F=> b
_4bitAdderProof5  = nil
_4bitAdderProof6  :: Adder4 '(F, T, T, F)  '(F, F, T, F) T '(T, F, F, T)  F=> b
_4bitAdderProof6  = nil
_4bitAdderProof7  :: Adder4 '(F, F, F, T)  '(F, T, T, T) F '(T, F, F, F)  F=> b
_4bitAdderProof7  = nil
_4bitAdderProof8  :: Adder4 '(F, F, F, T)  '(F, T, T, T) T '(T, F, F, T)  F=> b
_4bitAdderProof8  = nil
_4bitAdderProof9  :: Adder4 '(T, T, T, T)  '(T, T, T, T) F '(T, T, T, F)  T=> b
_4bitAdderProof9  = nil
_4bitAdderProof10 :: Adder4 '(T, T, T, T)  '(T, T, T, T) T '(T, T, T, T)  T=> b
_4bitAdderProof10 = nil
_4bitAdderProof11 :: Adder4 '(T, F, T, F)  '(T, F, T, F) F '(F, T, F, F)  T=> b
_4bitAdderProof11 = nil
_4bitAdderProof12 :: Adder4 '(T, F, T, T)  '(T, F, T, T) F '(F, T, T, F)  T=> b
_4bitAdderProof12 = nil
_4bitAdderProof13 :: Adder4 '(T, T, F, F)  '(T, T, F, F) F '(T, F, F, F)  T=> b
_4bitAdderProof13 = nil
_4bitAdderProof14 :: Adder4 '(T, T, F, T)  '(T, T, F, T) F '(T, F, T, F)  T=> b
_4bitAdderProof14 = nil
_4bitAdderProof15 :: Adder4 '(T, T, T, F)  '(T, T, T, F) F '(T, T, F, F)  T=> b
_4bitAdderProof15 = nil
_4bitAdderProof16 :: Adder4 '(F, T, F, T)  '(F, T, F, T) T '(T, F, T, T)  F=> b
_4bitAdderProof16 = nil
_4bitAdderProof17 :: Adder4 '(F, F, F, T)  '(F, F, F, T) T '(F, F, T, T)  F=> b
_4bitAdderProof17 = nil

-- | The big one. Every byte of arithmetic the processor does goes through here,
--   so if this is wrong, everything is wrong, but at least consistently.
class Adder8 (a :: Byte) (b :: Byte) (c :: Axiom) (s :: Byte) (cout :: Axiom)  | a b c -> cout s

instance
    ( Adder4 '(a3, a2, a1, a0)
             '(b3, b2, b1, b0) c
             '(s3, s2, s1, s0) midh
    , Adder4 '(a7, a6, a5, a4)
             '(b7, b6, b5, b4) midh
             '(s7, s6, s5, s4) cout
    ) => Adder8
            '(a7, a6, a5, a4, a3, a2, a1, a0)
            '(b7, b6, b5, b4, b3, b2, b1, b0)
            c '(s7, s6, s5, s4, s3, s2, s1, s0) cout

_8bitAdderProof1 :: Adder8 '(F, F, T, F, T, F, T, F) '(F, F, F, F, T, T, T, T) F '(F, F, T, T, T, F, F, T) F => b
_8bitAdderProof1 = nil
_8bitAdderProof2 :: Adder8 '(T, T, T, T, T, T, T, T) '(F, F, F, F, F, F, F, T) F '(F, F, F, F, F, F, F, F) T => b
_8bitAdderProof2 = nil
-- 100 + 100, the only time this processor feels like a millionaire
_8bitAdderProof3 :: Adder8 '(F, T, T, F, F, T, F, F) '(F, T, T, F, F, T, F, F) F '(T, T, F, F, T, F, F, F) F => b
_8bitAdderProof3 = nil
_8bitAdderProof4 :: Adder8 '(F, F, F, F, F, F, F, F) '(F, F, F, F, F, F, F, F) T '(F, F, F, F, F, F, F, T) F => b
_8bitAdderProof4 = nil

-- | Add one. Half adders are enough because the other operand is a single bit,
--   no point bringing a full adder to a half adder fight.
--   The final carry falls off the end of the byte, which is how 255 + 1 = 0.
class Increment (a :: Byte) (s :: Byte) | a -> s
instance ( HalfAdder a0 T  c1 s0
         , HalfAdder a1 c1 c2 s1
         , HalfAdder a2 c2 c3 s2
         , HalfAdder a3 c3 c4 s3
         , HalfAdder a4 c4 c5 s4
         , HalfAdder a5 c5 c6 s5
         , HalfAdder a6 c6 c7 s6
         , HalfAdder a7 c7 _overflow s7
         ) => Increment '(a7, a6, a5, a4, a3, a2, a1, a0) '(s7, s6, s5, s4, s3, s2, s1, s0)

incProof   :: Increment '(F, F, F, F, F, F, F, F) '(F, F, F, F, F, F, F, T) => b
incProof   = nil
incProof2  :: Increment '(F, F, F, F, F, F, F, T) '(F, F, F, F, F, F, T, F) => b
incProof2  = nil
incProof16 :: Increment '(F, F, F, F, T, T, T, F) '(F, F, F, F, T, T, T, T) => b
incProof16 = nil
incProofLots :: Increment '(T, T, T, T, T, T, T, F) '(T, T, T, T, T, T, T, T) => b
incProofLots = nil
incProofOverflow :: Increment '(T, T, T, T, T, T, T, T) '(F, F, F, F, F, F, F, F) => b
incProofOverflow = nil

-- | Subtract one, by adding 255 and pretending the carry never happened.
class Decrement (a :: Byte) (s :: Byte) | a -> s
instance Adder8 a '(T, T, T, T, T, T, T, T) F s _carry => Decrement a s

decProof :: Decrement '(F, F, F, F, F, F, F, T) '(F, F, F, F, F, F, F, F) => b
decProof = nil
decProofUnderflow :: Decrement '(F, F, F, F, F, F, F, F) '(T, T, T, T, T, T, T, T) => b
decProofUnderflow = nil
decProofBorrow :: Decrement '(F, F, F, T, F, F, F, F) '(F, F, F, F, T, T, T, T) => b
decProofBorrow = nil

-- | a - b = a + not b + 1. The + 1 sneaks in through the carry, so there's no
--   need for a separate increment like the 16 bit version used to do.
class Subtract (a  :: Byte) (b  :: Byte) (s  :: Byte) | a b -> s
instance (
         Invert b bout
         , Adder8 a bout T s _carry
         ) => Subtract a b s

subtractProof0  :: Subtract '(F, F, F, F, F, F, F, F) '(F, F, F, F, F, F, F, F) '(F, F, F, F, F, F, F, F) => b
subtractProof0  = nil
subtractProof1  :: Subtract '(F, F, F, F, F, F, F, T) '(F, F, F, F, F, F, F, F) '(F, F, F, F, F, F, F, T) => b
subtractProof1  = nil
subtractProof2  :: Subtract '(F, F, F, F, F, F, F, T) '(F, F, F, F, F, F, F, T) '(F, F, F, F, F, F, F, F) => b
subtractProof2  = nil
subtractProofOVerflow  :: Subtract '(F, F, F, F, F, F, F, T) '(F, F, F, F, F, F, T, F) '(T, T, T, T, T, T, T, T) => b
subtractProofOVerflow  = nil
subtractProofOVerflow2 :: Subtract '(F, F, F, F, F, F, F, T) '(F, F, F, F, F, F, T, T) '(T, T, T, T, T, T, T, F) => b
subtractProofOVerflow2 = nil
subtractProof22 :: Subtract '(F, F, F, F, F, F, T, F) '(F, F, F, F, F, F, T, F) '(F, F, F, F, F, F, F, F) => b
subtractProof22 = nil
subtractProof30 :: Subtract '(F, F, F, F, T, F, F, F) '(F, F, F, F, T, F, F, F) '(F, F, F, F, F, F, F, F) => b
subtractProof30 = nil
subtractProof40 :: Subtract '(T, F, F, F, F, F, F, F) '(T, F, F, F, F, F, F, T) '(T, T, T, T, T, T, T, T) => b
subtractProof40 = nil
-- 42 - 7, the answer to life minus a week
subtractProof42 :: Subtract '(F, F, T, F, T, F, T, F) '(F, F, F, F, F, T, T, T) '(F, F, T, F, F, F, T, T) => b
subtractProof42 = nil

class IsZero4 (a  :: (Axiom, Axiom, Axiom, Axiom)) (s :: Axiom) | a -> s

instance (
          Or a3 a2 o1
         , Or a1 a0 o2
         , Or o1 o2 o3
         , Not o3 s
         ) => IsZero4 '(a3, a2, a1, a0) s

-- | Is it nothing? Asks every bit, any single one of them can veto.
class IsZero (a  :: Byte) (s :: Axiom) | a -> s

instance (
          IsZero4 '(a7, a6, a5, a4) high
         , IsZero4 '(a3, a2, a1, a0) low
         , And high low s
         ) => IsZero '(a7, a6, a5, a4, a3, a2, a1, a0) s

zeroProof :: IsZero '(F, F, F, F, F, F, F, F) T => b
zeroProof = nil
zeroProof1 :: IsZero '(F, F, T, F, F, F, F, F) F => b
zeroProof1 = nil
zeroProof2 :: IsZero '(T, F, F, F, F, F, F, F) F => b
zeroProof2 = nil
zeroProof3 :: IsZero '(F, F, F, F, F, F, F, T) F => b
zeroProof3 = nil

-- | Two's complement: the top bit is the sign. No gates required,
--   it's just a wire with an opinion.
class IsNegative (a :: Byte) (s :: Axiom) | a -> s
instance IsNegative '(a7, a6, a5, a4, a3, a2, a1, a0) a7

negativeProof :: IsNegative '(T, F, F, F, F, F, F, F) T => b
negativeProof = nil
negativeProof2 :: IsNegative '(F, T, T, T, T, T, T, T) F => b
negativeProof2 = nil

-- | Equal bytes have no bits that disagree.
--   The processor uses this to notice a program jumping onto itself forever.
class Equal (a :: Byte) (b :: Byte) (s :: Axiom) | a b -> s
instance ( ByteXor a b disagreements
         , IsZero disagreements s
         ) => Equal a b s

equalProof :: Equal '(T, F, T, F, F, F, T, T) '(T, F, T, F, F, F, T, T) T => b
equalProof = nil
equalProof2 :: Equal '(T, F, T, F, F, F, T, T) '(T, F, T, F, F, F, T, F) F => b
equalProof2 = nil
equalProof3 :: Equal '(F, F, F, F, F, F, F, F) '(T, F, F, F, F, F, F, F) F => b
equalProof3 = nil

x :: Int -- inhabitation means proved
x = halfAdderProof1
    halfAdderProof2
    halfAdderProof3
    halfAdderProof4
    adderProof1
    adderProof2
    adderProof3
    adderProof4
    adderProof5
    adderProof6
    adderProof7
    adderProof8
    _2bitAdderProof1
    _2bitAdderProof2
    _2bitAdderProof3
    _2bitAdderProof4
    _2bitAdderProof5
    _2bitAdderProof6
    _2bitAdderProof7
    _2bitAdderProof8
    _2bitAdderProof9
    _2bitAdderProof10
    _4bitAdderProof3
    _4bitAdderProof4
    _4bitAdderProof5
    _4bitAdderProof6
    _4bitAdderProof7
    _4bitAdderProof8
    _4bitAdderProof9
    _4bitAdderProof10
    _4bitAdderProof11
    _4bitAdderProof12
    _4bitAdderProof13
    _4bitAdderProof14
    _4bitAdderProof15
    _4bitAdderProof16
    _4bitAdderProof17
    _8bitAdderProof1
    _8bitAdderProof2
    _8bitAdderProof3
    _8bitAdderProof4
    incProof
    incProof2
    incProof16
    incProofLots
    incProofOverflow
    decProof
    decProofUnderflow
    decProofBorrow
    subtractProof0
    subtractProof1
    subtractProof2
    subtractProofOVerflow
    subtractProofOVerflow2
    subtractProof22
    subtractProof30
    subtractProof40
    subtractProof42
    zeroProof
    zeroProof1
    zeroProof2
    zeroProof3
    negativeProof
    negativeProof2
    equalProof
    equalProof2
    equalProof3
