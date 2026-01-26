This example is derived from the S9 scheme virtual machine 
downloaded on 2026-01-21 from [here](https://t3x.org/files/s9fes-2014.tgz).

The book [Scheme 9 from Empty Space](https://t3x.org/s9book/) explains the
source code. The Micron migration tries to be faithful to the original function and variable names.
The logic inside the procedures is nearly line-for-line identical to the book's explanations.

Differences to the book:

- Relies on C's stdio ability to put a character back into the stream (ungetc) during parsing. Micron Files does not support this.
- error() often implies a "reset" of the interpreter, jumping back to the REPL. In C, this uses setjmp/longjmp or careful return value checking.
  Micron uses a global S9Def.ErrorFlag. Every loop (Evaluator, Reader) checks IF ErrorFlag THEN EXIT END. The flow is more explicit in Micron than in the C code.
- Variables like Stack or Car are available everywhere. Micron uses S9Def. prefixes everywhere (e.g., S9Def.Car[n]). This is due to Micron's strict modularity.

THIS IS PENDING WORK - more to come
