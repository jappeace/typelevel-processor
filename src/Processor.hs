{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
-- GHC calls Assemble and Run redundant constraints. They're the entire computer.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | An 8 bit processor at the type level, built from nothing but nand,
--   for executing fizzbuzz programs (and other, lesser programs).
--
-- Write a program with the "Assembler" statements and hand it to 'execute'.
-- The program runs while your code compiles, the executable merely reads
-- out what the type checker already worked out.
-- Running long programs needs @{-\# OPTIONS_GHC -freduction-depth=0 \#-}@
-- in the module calling 'execute', every clock cycle nests a little deeper.
module Processor
  ( execute
  , executeWithin
  , DefaultCycleLimit
  , Snapshot(..)
  , readRam
  , Statement(..)
  , Target(..)
  , Source(..)
  , Expression(..)
  , Jump(..)
  )
where

import GHC.TypeLits (Nat, KnownNat, natVal)
import Data.Proxy (Proxy(..))
import Assembler
import Computer
import Reflection

-- | Run a program from power on until it halts, and report what's left.
--   Gives up with a type error after 'DefaultCycleLimit' cycles.
--
-- >>> registerD (execute @'[ 'Literal 2, 'Assign '[ 'ToD] ('Y 'A) 'Never, 'Literal 40, 'Assign '[ 'ToD] ('DPlus 'A) 'Never, 'Halt ])
-- 42
execute :: forall program rom cycles final.
  ( Assemble program rom
  , Run rom PowerOn DefaultCycleLimit cycles final
  , KnownNat cycles
  , KnownComputer final
  ) => Snapshot
execute = executeWithin @DefaultCycleLimit @program

-- | Every cycle costs the type checker about a tenth of a second,
--   so this is roughly two minutes of compiling, which felt like plenty.
type DefaultCycleLimit = 1000

-- | 'execute' with a cycle limit of your choosing, for programs with ambition.
executeWithin :: forall (limit :: Nat) program rom cycles final.
  ( Assemble program rom
  , Run rom PowerOn limit cycles final
  , KnownNat cycles
  , KnownComputer final
  ) => Snapshot
executeWithin = snapshotAfter @final (fromInteger (natVal (Proxy @cycles)))
