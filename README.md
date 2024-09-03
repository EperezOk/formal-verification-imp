# Formal Verification of an Imperative Language

This project is a Haskell implementation of a simple imperative language (IMP) parser and interpreter, along with a Z3 solver that can verify invariants in the IMP programs. It includes:

- A simple imperative language (IMP) interpreter, which can run IMP programs and output their final state.
- A parser for the IMP language.
- A compiler that turns and IMP AST into a Z3 AST, while adding constraints to the Z3 solver.
- A Z3 solver that can constrain the input variables of an IMP program and assert that certain invariants hold in the final state.

## Table of Contents

- [Formal Verification of an Imperative Language](#formal-verification-of-an-imperative-language)
  - [Table of Contents](#table-of-contents)
  - [Setup](#setup)
  - [Usage](#usage)
    - [Running the IMP interpreter](#running-the-imp-interpreter)
    - [Running the Z3 solver](#running-the-z3-solver)
      - [Overflow Example](#overflow-example)
      - [Greatest Common Divisor Example](#greatest-common-divisor-example)
- [Acknowledgements](#acknowledgements)

## Setup

1. Install Z3 from the [official repository](https://github.com/Z3Prover/z3).
   
   - Pre-built binaries are available for Windows, Linux and macOS [here](https://github.com/Z3Prover/z3/releases).
   - Bug fixing: [1](https://stackoverflow.com/questions/73279979/installing-z3-haskell-bindings-on-windows), [2](https://cabal.readthedocs.io/en/3.4/cabal-project.html#foreign-function-interface-options), [3](https://stackoverflow.com/questions/77253044/installing-z3-binding-with-haskell-on-mac).

2. Set the path to the Z3 binary in the [`.cabal` file](formal-verification-imp.cabal), under the `include-dirs` and `extra-lib-dirs` fields.
3. Add the `z3` executable inside the `bin` directory to the system PATH.
4. Build the project.

    ```bash
    cabal build
    ```

## Usage

You can run the `main` function in the main module by using the following command:

```bash
cabal run
```

Alternatively, you can open a GHCi session to try out the functions interactively:

```bash
cabal repl
```

> The repl is useful for testing the examples described below.

### Running the IMP interpreter

The interpreter needs that all variables are defined within the program; otherwise a runtime error will be thrown.

You can try running the following function:

```hs
runInterpreter "examples/gcd.imp"
```

This will output both the AST for that program, and the final state of the program after running it.

```text
Seq (Seq (Seq (Seq (Seq Skip (Set a (Lit 419990535))) (Set b (Lit 202590585))) (Set d (Lit 0))) (While ((((Var a :/: Lit 2) :*: Lit 2) :==: Var a) ...)))

[(b,135),(a,135),(d,0)]
```

### Running the Z3 solver

With the Z3 solver, we can leave some variables uninitialized in the program, and the solver will try to find a solution for them.

#### Overflow Example

In this example, we'll demonstrate how we can use the Z3 solver to check invariants in a program.

We'll use the [`overflow-z3.imp`](examples/overflow-z3.imp) IMP program from the `examples` directory:

```imp
balance := 0;
bonus := 10;

if (1000 <= deposit) {
    balance := balance + deposit * bonus
} else {
    balance := balance + deposit
}
```

Here, we have a free variable `deposit`, and we want to assert that the `balance` variable will always be positive after the program runs. For this, we'll impose two constraints on the state of the program:

1. The initial `deposit` variable must be non-negative.
2. The final `balance` variable must be **negative**.

If the solver finds a solution for these constraints, it means that the program can reach a state where the `balance` variable is negative, thus breaking the invariant `balance >= 0`. Otherwise, the solver will return `Unsat`, meaning that the invariant holds (ie. it is impossible to assign a value to the free variable `deposit` that will make the `balance` variable negative at the end of the program execution).

Running the solver for the `overflow-z3.imp` program:

```hs
runZ3OverflowExample "examples/overflow-z3.imp"
```

We'll get a solution for the deposit variable that makes the balance negative (ie. breaks the invariant):

```text
deposit!0 -> #x7fffffff
...
```

This happens due to an overflow in the `balance` variable, which causes it to become negative. You can verify this by running the program with the given solution in the interpreter:

```hs
runInterpreter "examples/overflow.imp" 
```

```text
[(balance,-10),(bonus,10),(deposit,2147483647)]
```

We can also run the solver with the same constraints for a program with the bug fixed, [`overflow-z3-fixed.imp`](examples/overflow-z3-fixed.imp):

```hs
runZ3OverflowExample "examples/overflow-z3-fixed.imp"
```

Here, the solver will return `Unsat`, meaning that the invariant `balance >= 0` holds for all possible values of the `deposit` variable.

```text
Unsat
```

#### Greatest Common Divisor Example

For this example, we'll use the [`gcd-z3.imp`](examples/gcd-z3.imp) IMP program from the `examples` directory, and impose the final constraints `a == 135`, `b == 135`, `d == 0`.

```hs
runZ3GcdExample "examples/gcd-z3.imp"
```

If we run the solver and search for the `a!0`, `b!1`, and `d!2` variables in the output, we can find the following solution:

```text
...
a!0 -> #x19088c07
...
b!1 -> #x0c134979
...
d!2 -> #x00000000
...
```

Which represents an initial state where `a == 419990535`, `b == 202590585`, and `d == 0`.

We can check that this is a valid solution by setting those initial values in the [`gcd.imp`](examples/gcd.imp) program, and running it through the IMP interpreter as shown in [Running the IMP interpreter](#running-the-imp-interpreter).

That should output a final state in which `a == 135`, `b == 135`, and `d == 0`, just as we wanted.

```text
[(b,135),(a,135),(d,0)]
```

# Acknowledgements

This project is based on the [IMP](https://github.com/TikhonJelvis/imp) repository by [Tikhon Jelvis](https://github.com/TikhonJelvis).
