TypeLevel
=========

Type-level programming is performing computations at compile-time with inputs
and outputs being types. In Haskell, type-level programming is achieved using
multi-parameter typeclasses.

Type-level programming is also related to template metaprogramming in C++, in
which factorials and even Turing machines can be computed at compile-time.

In F#, member constraints on statically resolved type parameters can be
exploited to achieve type-level programming.

This F# library contains type-level datatypes including natural numbers and
lists.

Basic Features
==============

 - Type-level functional programming
   - Application: `f <|- x`
   - Function composition: `<-<`
   - Functional programming on pairs: `Fork`, `Fst`, `Snd`
 - Natural nubmers
   - Addition and subtraction
   - Comparison
   - Repeated function application : `<|*- `
 - Lists
   - Maps, filter and fold

Type-Level Functional Programming
=================================

A type-level function defines the function application operator, `<|-`.
For example, the expression `IsZero <|- S(Z)` has type of `False`.

Because it is almost impossible to implement lambda calculus in type-level
programming, more complex type-level functions are written in point-free style
using combinators including the composition operator `<-<`.

Some function take pairs (2-tuples) as parameters. The Fork (`<*>`) combinator
creates a pair from two functions: `(f <*> g) <|- x === (f <|- x, g <|- x)`.

The `Partial(f, x)` combinator partically applies `f` with one argument:
`Partial(f, x) <|- y = f <|- (x, y)`.

Simple Instruction Set
======================

The `Comp` module implements an instruction set on type-level. The computation
model consists of a tape for code and a tape for data.

A tape is a list with one of the elements selected. It takes constant time to
shift left or right on a tape.

The `App(f)` instruction applies the function `f` on the current item of the
data tape, and overwrites that item.

The `CondJmp(f)` instruction performs a jump (applies `f` to the instruction
tape) if the current item of the data tape is true (checked using the `!?`
operator).

The `Jmp(f)` instruction performs a jump unconditionally.

