A Verified JIT for X86
======================

This project is based on the paper [Verified Just-In-Time Compiler on
x86](https://www.cl.cam.ac.uk/~mom22/jit/jit.pdf) by [Magnus
Myreen](https://www.cl.cam.ac.uk/~mom22/).

We implement a sound, incomplete analysis that ensures that the
just-in-time compiled machine code corresponding to a bytecode program
maintains the value stack (the stack doesn't underflow, or overflow
and end up accessing out-of-bounds memory).  This analysis is
performed on the generated assembly instructions.

We work under partial-correctness (as does the original paper):
programs may not terminate, but we ensure that if they do terminate
and pass our analysis, they wont go wrong.

More formally, we prove that given a bytecode string, an initial stack
(both supplied as command-line arguments) and a maximum stack size
(hardcoded in Main.ml), the generated native machine code maintains
the stack (the pointer to the stack, stored in register `RDI`, is
always within bounds).

Specifically we prove that for all possible paths through the
generated assembly the path either ends in a jump back to the runtime
in a bounded number of steps without blowing or underflowing the stack
(corresponding to a Stop bytecode instruction), or the path results in
a loop where the stack size at the end of the loop is the same as at
the start.

The input is the simple stack based language from the paper.  For
example, the program "`=6<4-j0sj0.`" calculates the greatest common
divisor (GCD) of two numbers and corresponds to the bytecode:

```
0: Jeq 6
1: Jlt 4
2: Sub
3: Jump 0
4: Swap
5: Jump 0
6: Stop
```

There are 3 paths (the below indices are bytecode-instruction offsets;
the tool reports x86 instruction offsets):

```
[0, 1, 2, 3]
[0, 1, 4, 5]
[0, 6]
```

The following example will fail verification if there are less than 4
items given as input to the stack:

```
ppp.
```

We use the semantics and Hoare logic from the paper.  Their Hoare
logic is actually an adaptation of separation logic, where instead of
thinking of the heap as disjoint partial functions, you can think of
machine registers and eflags (condition codes) as disjoint partial
functions.  This allows us to implement a very simple verification
function that ignores all instructions that don't have to do with
modifying register `RDI` (where the pointer to the stack lives).


BUILDING
--------

This project builds under macOS and Linux.  Note: on Linux, you must
use the Gold linker, as the old BFD linker doesn't like linking
against absolute paths. (on Fedora: `sudo alternatives --set ld
/usr/bin/ld.gold`)

To build, make sure you have ocaml and z3 installed and on your path.
On macOS this is:

```sh
$ brew install ocaml opam z3
```


Then:

```sh
$ opam repository add plasma-opam https://github.com/plasma-umass/opam-repository.git
$ make setup # installs required libraries
$ make       # builds and runs tests
```

From here, you can test out different programs like:

```sh
$ ./Main.native 'pp.' 1
```


LICENSE
-------

Licensed under the ISC license.
