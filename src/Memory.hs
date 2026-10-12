{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Memory where

import Plumbing
import LogicGates
import Bytewise
import Arithmetics

class FirstOrFalse (input :: [Axiom]) (out :: Axiom ) | input -> out
instance FirstOrFalse '[] F
instance FirstOrFalse (x ': _rem) x

-- | A latch component stores and outputs a single bit
--
-- When st (store) is 1, the value on d is stored and emitted.
--
-- When st is 0, the value of d is ignored, and the previously stored value is still emitted.
--
-- we use a list of axioms to indicate the passage of time, where every item is a tick
-- first item is processed last. in [a,b,c], c will be processed firt, then b, then a
class Latch (input :: [ ( Axiom, Axiom ) ] ) (out :: [Axiom]) | input -> out

instance Latch '[] '[]
instance (
          FirstOrFalse prevOut prevMem
        , Latch rem prevOut
        , LatchTick st d prevMem out
        ) => Latch ('(st, d) ': rem )  (out ': prevOut)

-- | A single tick of the latch, for circuits that carry their own past around
--   instead of replaying all of history every time someone asks.
--   A latch is a selector that listens to its own output, the feedback wire is
--   whoever passes the previous output back in.
class LatchTick (st :: Axiom) (d :: Axiom) (stored :: Axiom) (out :: Axiom) | st d stored -> out
instance Selector st d stored out => LatchTick st d stored out

latchStProof1 :: Latch '[ '( T, F)] '[F] => b
latchStProof1 = nil
latchStProof2 :: Latch '[ '( T, T)] '[T] => b
latchStProof2 = nil

latchSetTrueMem :: Latch '[ '( F, F), '( T, T)] '[T , T] => b
latchSetTrueMem = nil
latchSetTrueMem2 :: Latch '[ '( F, F), '( F, F), '( T, T)] '[T, T, T] => b
latchSetTrueMem2 = nil

latchSetTrueFalseMem :: Latch '[ '( F, T), '( T, F), '( F, F), '( T, T)] '[F, F, T , T] => b
latchSetTrueFalseMem = nil


class Zip (ina :: [a]) (inb :: [b]) (out :: [(a,b)]) | ina inb -> out
instance Zip '[] b '[]
instance (
    Zip nexta nextb rest
         ) => Zip (ina ': nexta) (inb ': nextb) ('(ina, inb) ': rest)

class Third (input :: [(a,b,c)]) (out :: [c]) | input -> out
instance Third '[] '[]
instance (Third rest out)
          => Third ( '(_a, _b, c) ': rest) ( c ': out)

class HalfFlop (input :: [ (Axiom, Axiom, Axiom) ]) (out :: [(Axiom, Axiom)]) | input -> out
instance HalfFlop '[] '[]
instance
  ( HalfFlop rem prevOut
  , Not cl cln't
  , And st cln't lst
  )
  => HalfFlop ('(st, d, cl) ': rem) ('(lst, d) ': prevOut)

-- | A data flip-flop: two latches in series, inputs are @(st, d, cl)@ ticks.
--
-- While cl is 0 the first latch stores d when st is 1, the second latch holds.
-- While cl is 1 the second latch copies the first one, so the output only
-- changes when the clock goes up.
class FlipFlop (input  :: [ (Axiom, Axiom, Axiom) ]) (out :: [Axiom]) | input -> out
instance (
          HalfFlop input hflops
        , Latch hflops ds
        , Zip cls ds zipped
        , Latch zipped out
        , Third input cls
        ) => FlipFlop input  out

flipFlopNothingProof1 :: FlipFlop '[ '(F, F, F)] '[F] => b
flipFlopNothingProof1 = nil

flipFlopNothingProof2 :: FlipFlop '[ '(T, T, F)] '[F] => b
flipFlopNothingProof2 = nil

flipFlopNothingProof3 :: FlipFlop '[ '(T, T, T)] '[F] => b
flipFlopNothingProof3 = nil

flipFlopSetProof1 :: FlipFlop '[ '(F, F, F),  '(F, F, T), '(T, T, F)] '[ T, T, F] => b
flipFlopSetProof1 = nil

-- storing while the clock is low must not reach the output until the clock rises
flipFlopHoldUntilClockProof :: FlipFlop '[ '(F, F, T), '(T, F, F), '(F, F, T), '(T, T, F)] '[ F, T, T, F] => b
flipFlopHoldUntilClockProof = nil

-- | One full clock cycle of the flip-flop above: the clock goes low, then high.
--   See Note [Folding the clock].
class FlipFlopCycle (st :: Axiom) (d :: Axiom) (stored :: Axiom) (next :: Axiom) | st d stored -> next
instance LatchTick st d stored next => FlipFlopCycle st d stored next

-- Note [Folding the clock]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
-- Over one whole cycle the flip-flop's two latches do this:
--
--   clock low:  first latch stores d if st is on, second latch holds
--   clock high: first latch holds, second latch copies the first
--
-- So after the cycle both latches hold "st ? d : stored", which is exactly one
-- latch tick. The first version spelled out both phases with four latch ticks
-- per bit, and the type checker spent a third of every clock cycle confirming
-- that holding still holds things still.
--
-- Nobody has to take my word for it: 'CycleMatchesFlipFlop' runs the real two
-- latch 'FlipFlop' for every combination of stored, st and d and demands the
-- same answer.

-- | The proof obligation for Note [Folding the clock]. history is whatever
--   earlier ticks got the flip-flop to hold stored.
class CycleMatchesFlipFlop (history :: [(Axiom, Axiom, Axiom)]) (stored :: Axiom) (st :: Axiom) (d :: Axiom)
instance
  ( FlipFlop history (stored ': _earlier)
  , FlipFlop ('(st, d, T) ': '(st, d, F) ': history) (next ': _rest)
  , FlipFlopCycle st d stored next
  ) => CycleMatchesFlipFlop history stored st d

type HoldingLies = '[ '(T, F, T), '(T, F, F)]
type HoldingTruth = '[ '(T, T, T), '(T, T, F)]

-- all eight of them, there's no hiding in a truth table
flipFlopCycleMatches :: ( CycleMatchesFlipFlop HoldingLies F F F
                        , CycleMatchesFlipFlop HoldingLies F F T
                        , CycleMatchesFlipFlop HoldingLies F T F
                        , CycleMatchesFlipFlop HoldingLies F T T
                        , CycleMatchesFlipFlop HoldingTruth T F F
                        , CycleMatchesFlipFlop HoldingTruth T F T
                        , CycleMatchesFlipFlop HoldingTruth T T F
                        , CycleMatchesFlipFlop HoldingTruth T T T
                        ) => b
flipFlopCycleMatches = nil

flipFlopCycleStoreProof :: FlipFlopCycle T T F T => b
flipFlopCycleStoreProof = nil
flipFlopCycleIgnoreProof :: FlipFlopCycle F T F F => b
flipFlopCycleIgnoreProof = nil
flipFlopCycleRememberProof :: FlipFlopCycle F F T T => b
flipFlopCycleRememberProof = nil
flipFlopCycleOverwriteProof :: FlipFlopCycle T F T F => b
flipFlopCycleOverwriteProof = nil

-- | Eight flip-flops sharing a store wire and a clock, holding a byte
--   hostage for one cycle at a time.
class Register (st :: Axiom) (d :: Byte) (stored :: Byte) (next :: Byte) | st d stored -> next
instance
  ( FlipFlopCycle st d7 s7 n7, FlipFlopCycle st d6 s6 n6, FlipFlopCycle st d5 s5 n5, FlipFlopCycle st d4 s4 n4
  , FlipFlopCycle st d3 s3 n3, FlipFlopCycle st d2 s2 n2, FlipFlopCycle st d1 s1 n1, FlipFlopCycle st d0 s0 n0
  ) => Register st
         '(d7, d6, d5, d4, d3, d2, d1, d0)
         '(s7, s6, s5, s4, s3, s2, s1, s0)
         '(n7, n6, n5, n4, n3, n2, n1, n0)

registerStoreProof :: Register T '(F, F, T, F, T, F, T, F) '(T, T, T, T, F, F, F, F) '(F, F, T, F, T, F, T, F) => b
registerStoreProof = nil
registerHoldProof :: Register F '(F, F, T, F, T, F, T, F) '(T, T, T, T, F, F, F, F) '(T, T, T, T, F, F, F, F) => b
registerHoldProof = nil

-- | A register with an increment strapped to it. Every cycle it either counts
--   up by one, or, when st is on, gets told to go somewhere else entirely.
--   Programs call that second thing a jump, the counter calls it a betrayal.
class Counter (st :: Axiom) (x :: Byte) (stored :: Byte) (next :: Byte) | st x stored -> next
instance
  ( Increment stored incremented
  , Select8 st x incremented input
  , Register T input stored next
  ) => Counter st x stored next

counterCountProof :: Counter F '(F, F, T, F, T, F, T, F) '(F, F, F, F, F, T, F, T) '(F, F, F, F, F, T, T, F) => b
counterCountProof = nil
counterJumpProof :: Counter T '(F, F, T, F, T, F, T, F) '(F, F, F, F, F, T, F, T) '(F, F, T, F, T, F, T, F) => b
counterJumpProof = nil
counterWrapProof :: Counter F '(F, F, T, F, T, F, T, F) '(T, T, T, T, T, T, T, T) '(F, F, F, F, F, F, F, F) => b
counterWrapProof = nil

x :: Int
x =
   latchStProof1
   latchStProof2
   latchSetTrueMem
   latchSetTrueMem2
   latchSetTrueFalseMem
   flipFlopNothingProof1
   flipFlopNothingProof2
   flipFlopNothingProof3
   flipFlopSetProof1
   flipFlopHoldUntilClockProof
   flipFlopCycleMatches
   flipFlopCycleStoreProof
   flipFlopCycleIgnoreProof
   flipFlopCycleRememberProof
   flipFlopCycleOverwriteProof
   registerStoreProof
   registerHoldProof
   counterCountProof
   counterJumpProof
   counterWrapProof
