# Indigo Common Lisp

Concept implementation for higher-order extensions of Common Lisp like
* algebraic data types and strong static typing
* pattern matching definitions
* compile-time formal verification and static analysis

# Motivaton

The [original Lisp implementation was a 'low level' language](https://en.wikipedia.org/wiki/CAR_and_CDR#Etymology), and many of the CAR/CDR-using features of subsequent implementations that became Common Lisp were built for dedicated hardware like the Lisp Machines. Currently the vast majority of Lisp programmers are working on modern commodity hardware like x86, and compiler optimization requires type declarations.

Furthermore, Common Lispers today find their Lisp codebases increasingly interfacing with more external non-Lisp dependencies like databases and FFI systems which increases code complexity and room for error, unlike the monolithic Lisp systems of yesteryear. Manually writing type declarations, using primitive CL pattern matching mechanisms like DESTRUCTURING-BIND, and manually packaging error detection and condition management can result in long, messy (and buggy) code.

Languages built around type-driven development and pattern matching gain the following benefits:

* bugs and side effects captured at compile time = faster development of robust code
* algebraic data types + pattern matching = cleaner code
* easier formal verification of programs
* more reusable code and data through parametrics
* allows for more advanced constraint/logic programming
* if done right, better runtime performance

see Bob Krzaczek's ["Performance and Types in Lisp"](https://blog.30dor.com/2014/03/21/performance-and-types-in-lisp/)

# TODO
* fix issue with package scope of SUBTYPES in DATA macro
* build + test passing workflow
* partial eval compose + check composed func type
* literal and wildcard pattern match in PATTERN-REWRITE
* parametric types, range/native CL compound types
* custom CHECK-TYPE error messages for GADTs
* more advanced type inference (unification), backend Prolog unifier? type predicates?
* &rest native function typing https://stackoverflow.com/questions/19485248/what-do-optional-and-rest-mean-in-a-values-type-specifier
* record format + accessors for data definitions
* full tests, type check failure tests
* tutorial in readme
* clean up DATA macro
* emacs mode, definition syntax/type capitals highlight/indent
* Total Function verification
* more advanced features from Haskell definitions
* profile generated functions
* function def type inference