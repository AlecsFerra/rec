# Rec

A denotational semantic based interpreter for the `REC` programming language described in the 9th chapter of the book `“The formal semantics of programming languages” by G. Winskel, The MIT Press, 1993.`.

It relies on the Kleene-Knaster-Tarski fixpoint iteration sequence for evaluating programs.

The interpreter can perform lazy and strict evaluation of programs.

## How to use it

### Using stack

Run a program:
```
stack run -- path/to/src
```

Run a program using lazy evaluation:
```
stack run -- path/to/src -l
```

You can force the strict evaluation with the flag `-s` but it's already the default.

## Dependencies

None.
