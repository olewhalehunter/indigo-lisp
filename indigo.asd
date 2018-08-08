(asdf:defsystem #:indigo
    :description "Algebraic Data Types, Pattern Matching, and Strong Static Typing on Common Lisp."
    :author "Anders Puckett <andersenpuckett@gmail.com>"
    :license "GNU AGPLv3"
    :serial t
    ;; :depends-on (#:binary-types)
    :components ((:file "indigo")
		 (:file "tests")
		 )
    )
