# Change log for typelevel-processor

## 1.0.0

The gates are now an actual computer: an 8 bit processor after
nandgame's design that runs programs while GHC type checks them, with
a type level assembler to write those programs in and `execute` to read
the result back as values. The executable prints fizzbuzz from 1 to 15
as worked out by that processor.

Upgrading breaks two things. Everything that was 16 bits is 8 bits now:
`Increment`, `Invert`, `Subtract`, `IsZero` and `IsNegative` take a
`Byte`, and `Adder16` and `Bit16` are gone. The package is called
`typelevel-processor` instead of `template`, and builds with cabal and
npins instead of stack and hpack.

* An 8 bit processor running fizzbuzz in the type checker, plus the
  flip-flop fix it needed
  [#1](https://github.com/jappeace/typelevel-processor/pull/1)

## 0.0.0

import [template](https://github.com/jappeace/haskell-template-project).
