# SNUPL 2022-1

Assignments in **SNU CSE Programming Language (4190.310)** lecture, at 2022 spring.

All the codes are written in **OCaml**, version 4.02.3.

## [HW1]()
6 basic problems to be familiar with OCaml programming language.

To compile and run all testcases:

```
$ cd HW1
$ make test
```

## [HW2]()
6 other basic problems to be familiar with OCaml programming language.

To compile and run all testcases:

```
$ cd HW2
$ make test
```

## [HW3]()
Make interpreter of [K--]() language.

To compile the interpreter and run all testcases:

```
$ cd HW3/K__skel
$ ./test.sh
```

## [HW4]()
### Ex1: K- Interpreter
Make interpreter of [K-]() language.

To compile the interpreter and run all testcases:

```
$ cd HW4/K_skel
$ ./test.sh
```

### Ex2 & 3: K- Programming
Do some programming in K- language, then run them with the K- interpreter that I just made.

To run all testcases:
```
$ cd HW4
$ ./test.sh
```

### Ex4: A Pleasant Worry
Simplified version of a polymorphic type decision problem.

To run all testcases:

```
$ cd HW4
$ make test
```

### Ex5: Preparation for Exploration
Simplified version of an algorithm for solving type equations.

To run all testcases:

```
$ cd HW4
$ make test
```

## [HW5]()
Writing an essay about the origins of two big sphere of gravity in PL world: 
- imperative (Turing machine)
- functional (lambda calculus)

## [HW6]()
### Ex1: SM5
Make translator, which translates K- program to SM5 program.

### Ex2: Garbage Collection in SM5
Implement GC in SM5 interpreter.

When memory hits the limit, stop executing program and collect only reachable memories in current state.

To test Ex1 & 2:
```
$ cd HW6/SM5_skel
$ make test
```

### Ex3: Lambda Ground
Make interpreter of lambda calculus language, following normal-order reduction rule.

To run all testcases:
```
$ cd HW6/Lambda_skel
$ ./test.sh
```

## [HW7]()
### Ex1: CPS - Continuation Passing Style
Make translator, which translates lambda calculus expression into CPS expression.

To run all testcases:
```
$ cd HW7/CPS_skel
$ ./test.sh
```

### Ex2: Exception is a Sugar
Make translator, which removes every exception syntaxes(raise-handle) then replaces it with CPS expression.

To run all testcases:
```
$ cd HW7/Exn_skel
$ ./test.sh
```

### Ex3: M Interpreter
Make interpreter of [M]() language.

To compile the interpreter and run all testcases:

```
$ cd HW7/M_skel
$ ./test.sh
```

### Ex4: Lowfat M
Make type checking system, operating without running every program.

Implemented with M-algorithm.

To compile the type checking system and run all testcases:

```
$ cd HW7/Lowfat_skel
$ ./test.sh
```

## [HW8]()
### Ex1: SM5 RozettaX
Make translator, which translates SM5 program into Sonata program.

Sonata language is almost similar with SM5 program, except there is no 'K(continuation)' functionality.

To debug your Sonata code,
1. Compile RozettaX, by command `$ make`.
2. Run RozettaX with debug option and save output as a file, by command `$ ./run -debug input_file.sm5 > output_name.txt`.
3. Open `HW8/RozettaX_skel/debugger.html`.
4. Select output file.
5. With prev/next button, you can debug Sonata interpreter instruction-by-instruction.