[![https://jappieklooster.nl](https://img.shields.io/badge/blog-jappieklooster.nl-lightgrey)](https://jappieklooster.nl/tag/haskell.html)
[![Jappiejappie](https://img.shields.io/badge/twitch.tv-jappiejappie-purple?logo=twitch)](https://www.twitch.tv/jappiejappie)
[![Jappiejappie](https://img.shields.io/badge/discord-jappiejappie-black?logo=discord)](https://discord.gg/Hp4agqy)

# typelevel-processor

An 8 bit processor in Haskell's type system, built out of a single
`Nand` type class, running fizzbuzz while your code compiles.

```
$ cabal run exe
1
2
Fizz
4
Buzz
Fizz
7
8
Fizz
Buzz
11
Fizz
13
14
Fizz Buzz
```

That took the type checker 286 clock cycles and about 45 seconds.
By the time the executable runs, the processor has long since clocked
off; `main` merely prints what GHC already worked out.

Every gate is a type class with a functional dependency, and every gate
is made out of `Nand`, which is the one truth table taken on faith.
Solving a constraint is pushing current through a wire, and inhabiting
a type is proof the circuit does what it says.

## The machine

It's [nandgame](https://nandgame.com)'s computer with the data path cut
down to 8 bits:

* registers A and D, 8 bits each,
* 256 bytes of RAM, addressed by A, the byte there is called *A,
* a ROM of 256 instructions of 16 bits each, with an 8 bit program counter.

So data is 8 bits and instructions are 16, like an AVR. nandgame's
instructions need 13 control bits, and squeezing them into 8 would have
meant inventing a worse instruction set.

| bits    | meaning                                                        |
|---------|----------------------------------------------------------------|
| 15      | ci: on for a computation, off for "load the low byte into A"   |
| 12      | the y operand is *A instead of A                               |
| 10..6   | ALU knobs: u, op1, op0, zx, sw                                 |
| 5..3    | result goes to A, D, *A (any combination)                      |
| 2..0    | jump to A when the result is lt, eq, gt zero                   |

## The layers

Each module builds on the ones above it, and each one comes with proofs:
constraints like `Adder8 a b F sum carry => b` that the module forces GHC
to solve. If one of those is wrong, the library doesn't compile.

| module                | what's in it                                                       |
|-----------------------|--------------------------------------------------------------------|
| `LogicGates`          | `Nand`, and not, and, or, xor made from it                         |
| `Bytewise`            | the `Byte` kind and gates on eight wires at once                   |
| `Arithmetics`         | half and full adders up to 8 bits, increment, subtract, comparisons |
| `Plumbing`            | selectors and switches                                              |
| `Memory`              | latch, flip-flop, register, counter                                |
| `RandomAccessMemory`  | 256 bytes of RAM as a tree that grows where programs go             |
| `ArithmeticLogicUnit` | nandgame's logic unit, arithmetic unit, ALU and condition           |
| `ControlUnit`         | decodes an instruction and tells everyone what to do               |
| `Computer`            | fetch, one clock cycle, and running until the program halts        |
| `Assembler`           | type level assembly with labels, so nobody types 16 `Truth`s by hand |
| `Reflection`          | drags the final state out of the types and into values             |
| `Processor`           | `execute`, the front door                                          |
| `FizzBuzz`            | the program this was all for                                       |

## Writing programs

Programs are type level lists of statements, in nandgame's assembly
dialect. Here's six times seven, the hard way:

```haskell
type Multiply =
  '[ 'Literal 7                                    -- A = 7
   , 'Assign '[ 'ToD] ('Y 'A) 'Never               -- D = A
   , 'Literal 1                                    -- A = 1
   , 'Assign '[ 'ToAtA] 'D 'Never                  -- *A = D
   , 'Label "loop"
   , 'Literal 6
   , 'Assign '[ 'ToD] ('Y 'A) 'Never               -- D = 6
   , 'Literal 0
   , 'Assign '[ 'ToAtA] ('DPlus 'AtA) 'Never       -- *A = D + *A
   , 'Literal 1
   , 'Assign '[ 'ToD, 'ToAtA] ('YMinusOne 'AtA) 'Never  -- D, *A = *A - 1
   , 'AddressOf "loop"
   , 'Assign '[] 'D 'JGT                           -- D ; JGT
   , 'Halt
   ]

answer :: Word8
answer = readRam (execute @Multiply) 0 -- 42
```

The statements:

* `'Literal n` puts n in A, as long as it fits in a byte,
* `'AddressOf "label"` puts the label's address in A, for jumping there,
* `'Label "label"` names the next instruction and takes no room,
* `'Assign targets expression jump` is nandgame's `targets = expression ; jump`,
* `'Halt` jumps onto itself, which the processor takes as its cue to stop.

The expressions are everything the ALU can be talked into with x = D
and y = A or *A: `'Zero`, `'One`, `'MinusOne`, `'D`, `'Y`, `'NotD`,
`'NotY`, `'NegateD`, `'NegateY`, `'DPlusOne`, `'YPlusOne`, `'DMinusOne`,
`'YMinusOne`, `'DPlus`, `'DMinus`, `'YMinusD`, `'DAnd`, `'DOr` and
`'DXor`. Jumps are `'Never`, `'JGT`, `'JEQ`, `'JGE`, `'JLT`, `'JNE`,
`'JLE` and `'JMP`, comparing signed, so 128 and up count as negative.

`execute` gives up after 1000 cycles; `executeWithin @limit` takes a
different limit. A module that calls either needs
`{-# OPTIONS_GHC -freduction-depth=0 #-}`, since every clock cycle nests
the solver a little deeper.

## When it goes wrong

Mistakes are type errors, never a quiet wrong answer:

* a program that doesn't halt in time:
  "The program is still running and the cycle limit is used up."
* running past the last instruction:
  "The program counter ran off the end of the program."
* `'Literal 256`: "The literal 256 doesn't fit in a byte. It's an 8 bit processor, sir."
* jumping to a label that doesn't exist, or defining one twice,
* more than 256 instructions.

## Building

```
nix-shell
cabal build
cabal test
cabal run exe
```

Build with optimisation, which cabal does by default. Every solved
constraint becomes a dictionary in the generated code, and only the
optimiser notices that none of them are ever used. At `-O0` GHC
dutifully compiles the entire evidence that the processor works, which
roughly triples the build time.

A clock cycle costs the type checker about a tenth of a second and a
few megabytes, so fizzbuzz to 15 compiles in under a minute and peaks
around 1.5 GB. Fizzbuzz to 100 is left as an exercise for people with
more RAM than sense.
