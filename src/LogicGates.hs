{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Basic logic gates
module LogicGates where

data Axiom = Truth -- ^ when someone tells you the truth, like that.
            | Lies -- ^ when someone tells you the truth, but not.

type F = Lies -- ^ truthn't
type T = Truth

-- our only function
class Nand (in1 :: Axiom) (in2 :: Axiom) (out :: Axiom) | in1 in2 -> out

-- TODO express nand based off relais instead of as an axiom
-- a truth table
instance Nand Lies Lies Truth
instance Nand Lies Truth Truth
instance Nand Truth Truth Lies
instance Nand Truth Lies Truth

-- all other functions will be aliases
type Not input = (Nand input input)

-- shorthand for that other stuff no one cares about
nil :: a
nil = error "undefined"

-- tests be like this
notProof1 :: Not Truth Lies => b
notProof1 = nil
notProof2 :: Not Lies Truth => b
notProof2 = nil

-- type alias didn't work with partial application.
-- guess we just use more typeclasses
class And (input :: Axiom) (input2 :: Axiom) (out :: Axiom) | input input2 -> out
instance (Not x out, Nand input input2 x) =>
  And input input2 out
        -- note that we're not even a fundep

andProof1 :: And Truth Truth Truth => b
andProof1 = nil
andProof2 :: And Lies Truth Lies => b
andProof2 = nil
andProof3 :: And Lies Lies Lies => b
andProof3 = nil
andProof4 :: And Truth Lies Lies => b
andProof4 = nil

class Or (input :: Axiom) (input2 :: Axiom) (out :: Axiom) | input input2 -> out
instance (
         Not input ninput
         , Not input2 ninput2
         , Nand ninput ninput2 out
         ) =>
  Or input input2 out

orProof1 :: Or Truth Truth Truth => b
orProof1 = nil
orProof2 :: Or Lies Truth Truth => b
orProof2 = nil
orProof3 :: Or Lies Lies Lies => b
orProof3 = nil
orProof4 :: Or Truth Lies Truth => b
orProof4 = nil

-- when stuck I used nand game https://nandgame.com/#
-- it's a bit easier as a diagram
--
-- This used to be nand, or and and: six nands. Four will do, and since the type
-- checker bills by the gate, the processor runs faster on the cheap version.
class Xor (input :: Axiom) (input2 :: Axiom) (out :: Axiom) | input input2 -> out
instance ( Nand input input2 both
         , Nand input both notOnlyInput
         , Nand input2 both notOnlyInput2
         , Nand notOnlyInput notOnlyInput2 out
         ) =>
  Xor input input2 out

xorProof1 :: Xor Truth Truth Lies => b
xorProof1 = nil
xorProof2 :: Xor Lies Truth Truth => b
xorProof2 = nil
xorProof3 :: Xor Lies Lies Lies => b
xorProof3 = nil
xorProof4 :: Xor Truth Lies Truth => b
xorProof4 = nil

x :: Int -- inhabitation means proved
x = notProof1 -- since the values are irrelevant, we jam all proofs here
    notProof2
    andProof1
    andProof2
    andProof3
    andProof4
    orProof1
    orProof2
    orProof3
    orProof4
    xorProof1
    xorProof1
    xorProof2
    xorProof2
    xorProof3
    xorProof3
    xorProof4
    xorProof4
