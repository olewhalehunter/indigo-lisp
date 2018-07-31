# Indigo Common Lisp

Common Lisp with algebraic data types, strong static typing, and pattern patching definitions

# Motivaton for ADTs and Strong Typing

The [original Lisp implementation was a 'low level' language](https://en.wikipedia.org/wiki/CAR_and_CDR#Etymology), and many of the CAR/CDR-using features of subsequent implementations that became Common Lisp were built for dedicated hardware like the Lisp Machines. Currently the vast majority of Lisp programmers are working on modern commodity hardware like x86, and compiler optimization requires type declarations.

Furthermore, Common Lispers today find their Lisp codebases increasingly interfacing with more external non-Lisp dependencies like databases and FFI systems which increases code complexity and room for error, unlike the monolithic Lisp systems of yesteryear. Manually writing type declarations, using primitive CL pattern matching mechanisms like DESTRUCTURING-BIND, and manually packaging error detection and condition management can result in long, messy (and buggy) code.

Languages built around type-driven development and pattern matching gain the following benefits:

* bugs and side effects captured at compile time = faster development of robust code
* pattern matching + ADTs = cleaner code
* easier formal verification of programs
* more reusable code and data through parametrics
* allows for more advanced constraint/logic programming
* done right, better runtime performance

see Bob Krzaczek's ["Performance and Types in Lisp"](https://blog.30dor.com/2014/03/21/performance-and-types-in-lisp/)

# TODO
* finish ADT specification macros
* register parametric function type signatures
* test parametric function type checking
* custom CHECK-TYPE error messages for ADTs
* Total Function verification
* add more features from Haskell
* profile generated functions