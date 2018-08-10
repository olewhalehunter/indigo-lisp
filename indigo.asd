(asdf:defsystem :indigo
    :version "0.0.1"
    :description "Algebraic Data Types, Pattern Matching, and Strong Static Typing on Common Lisp."
    :author "Anders Puckett <andersenpuckett@gmail.com>"
    :license "GNU AGPLv3"
    :serial t
    :components ((:file "indigo"))
    )

(asdf:defsystem :indigo.tests
    :serial t
    :depends-on ("indigo" "prove")
    :components ((:file "indigo.tests"))
    )
    
