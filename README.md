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

# Short Term TODO

* parametric types (List, Graph)
* type inference of DEF's (unification with Gambol prolog or https://common-lisp.net/project/cl-unification/)
* compile time type checks in DEF during macroexpansion
* partial eval compose + check composed func type
* multi arg pattern match ((RGB r1 g1 b1) (RGB r2 g2 b2)) (RGB (+ r1 r2)...)
* literal and wildcard pattern match in PATTERN-REWRITE
* record format accessors for type definitions
* tutorial in readme
* native CL compound types/ranges

# Long Term TODO

* lisp compiler optimization https://stackovyerflow.com/questions/32321054/using-declare-type-but-still-have-safe-functions and compiler macro support https://github.com/Bike/compiler-macro
* &rest native function typing https://stackoverflow.com/questions/19485248/what-do-optional-and-rest-mean-in-a-values-type-specifier https://stackoverflow.com/questions/43010732/lisp-sbcl-declare-a-function-argument-to-be-a-list-of-a-certain-type-for-type-ch?rq=1
* clean up DATA macro
* emacs mode, definition syntax/type capitals highlight/indent
* Total Function verification
* profile generated functions
* &environment support
* formal verification of programs (atop gambol backend?)
* other features from Haskell