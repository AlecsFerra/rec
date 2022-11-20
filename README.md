# Rec

A denotational semantic based interpreter for the `REC` programming language described in the 9th chapter of the book `“The formal semantics of programming languages” by G. Winskel, The MIT Press, 1993.`.

It relies on the Kleene-Knaster-Tarski fixpoint iteration sequence for evaluating programs.

The interpreter can perform lazy and strict evaluation of programs.

## The language

A programs consists of many function declaration:
```
myFunction(param1, param2) = param1 + param2
```

And a main expression a the bottom:
```
myFunction(1, 2)
```

You can find some example programs in the directory `examples`.

## How to use it

### Building the project (using stack)

```
stack build
```

Then add the `rec-exe` binary in your path.

### Rinning a rec program

Run a program:
```
rec-exe path/to/src
```

Run a program using lazy evaluation:
```
rec-exe -- path/to/src -l
```

You can force the strict evaluation with the flag `-s` but it's already the default.

(You can also just replace the `rec-exe` command with `stack run --` if you are in the root directory of the project)

## Dependencies

None.
