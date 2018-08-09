(defpackage #:indigo
  (:use #:cl #:prove)
  (:documentation "Algebraic Data Types, Pattern Matching, and Strong Static Typing on Common Lisp.")
  (:export #:def
           #:data
	   #:def-valuetype
	   #:type-of-valuetype
	   #:type-signature
	   #:set-type-signature	   
	   #:check-type-s
	   #:check-type-list

	   #:run-tests
	   
	   #:pattern-rewrite
	   #:subst-cons-lambda
	   #:compose-format
	   #:c+
	   #:s+
	   #:subtypes
	   ))
