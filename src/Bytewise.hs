{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Gates on eight wires at once, because soldering a processor one bit
--   at a time is how you end up with a 1 bit processor.
module Bytewise where

import LogicGates

-- | Eight bits, most significant bit first.
--   256 values ought to be enough for anybody.
type Byte = (Axiom, Axiom, Axiom, Axiom, Axiom, Axiom, Axiom, Axiom)

-- | The byte equivalent of keeping your mouth shut.
type ZeroByte = '(F, F, F, F, F, F, F, F)

-- | Not, but eight of them. Also known as -x - 1 if you squint at two's complement.
class Invert (a  :: Byte) (s  :: Byte) | a -> s
instance (
   Not a7 s7, Not a6 s6, Not a5 s5, Not a4 s4,
   Not a3 s3, Not a2 s2, Not a1 s1, Not a0 s0
  )
    => Invert
    '(a7, a6, a5, a4, a3, a2, a1, a0)
    '(s7, s6, s5, s4, s3, s2, s1, s0)

class ByteAnd (a :: Byte) (b :: Byte) (s :: Byte) | a b -> s
instance (
   And a7 b7 s7, And a6 b6 s6, And a5 b5 s5, And a4 b4 s4,
   And a3 b3 s3, And a2 b2 s2, And a1 b1 s1, And a0 b0 s0
  )
    => ByteAnd
    '(a7, a6, a5, a4, a3, a2, a1, a0)
    '(b7, b6, b5, b4, b3, b2, b1, b0)
    '(s7, s6, s5, s4, s3, s2, s1, s0)

class ByteOr (a :: Byte) (b :: Byte) (s :: Byte) | a b -> s
instance (
   Or a7 b7 s7, Or a6 b6 s6, Or a5 b5 s5, Or a4 b4 s4,
   Or a3 b3 s3, Or a2 b2 s2, Or a1 b1 s1, Or a0 b0 s0
  )
    => ByteOr
    '(a7, a6, a5, a4, a3, a2, a1, a0)
    '(b7, b6, b5, b4, b3, b2, b1, b0)
    '(s7, s6, s5, s4, s3, s2, s1, s0)

class ByteXor (a :: Byte) (b :: Byte) (s :: Byte) | a b -> s
instance (
   Xor a7 b7 s7, Xor a6 b6 s6, Xor a5 b5 s5, Xor a4 b4 s4,
   Xor a3 b3 s3, Xor a2 b2 s2, Xor a1 b1 s1, Xor a0 b0 s0
  )
    => ByteXor
    '(a7, a6, a5, a4, a3, a2, a1, a0)
    '(b7, b6, b5, b4, b3, b2, b1, b0)
    '(s7, s6, s5, s4, s3, s2, s1, s0)

invProof :: Invert '(F, F, F, F, F, F, F, F) '(T, T, T, T, T, T, T, T) => b
invProof = nil
invProof2 :: Invert '(T, F, T, F, T, F, T, T) '(F, T, F, T, F, T, F, F) => b
invProof2 = nil

andByteProof :: ByteAnd '(T, T, F, F, T, F, T, F) '(T, F, T, F, T, T, F, F) '(T, F, F, F, T, F, F, F) => b
andByteProof = nil
orByteProof :: ByteOr '(T, T, F, F, T, F, T, F) '(T, F, T, F, T, T, F, F) '(T, T, T, F, T, T, T, F) => b
orByteProof = nil
xorByteProof :: ByteXor '(T, T, F, F, T, F, T, F) '(T, F, T, F, T, T, F, F) '(F, T, T, F, F, T, T, F) => b
xorByteProof = nil

x :: Int -- inhabitation means proved
x = invProof
    invProof2
    andByteProof
    orByteProof
    xorByteProof
