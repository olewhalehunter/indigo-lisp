;; (prove:run #P"test/tree.lisp" :reporter :tap)

(in-package :indigo)

(plan nil)

(subtest "Testing Definitions"
  (ok
   (data Tree
	 Empty
	 (Leaf Integer)
	 (Branch Tree Tree))
   )

  (ok
   (def depth
       Empty 0
       (Leaf n) 1
       (Branch l r) (+ 1 (max (depth l)
			      (depth r))))
   ))
  


(subtest "Testing Type Predicates"
  (ok (typep Empty 'Tree))
  (ok (typep (Leaf 4) 'Tree))
  (ok (typep (Branch (Leaf 2) (Leaf 4)) 'Tree))
  (ok (typep (Branch Empty Empty) 'Tree))
  )

(subtest "Testing Type Checks"
  (is-error (Leaf "foo") 'type-error)
  (is-error (Branch 1 1) 'type-error)
  (is-error (Branch Empty (Leaf "bar")) 'type-error)
  )

(subtest "Testing Pattern Matched Function"
  (is (depth Empty) 0)
  (is (depth (Leaf 7)) 1)
  (is (depth (Branch Empty Empty)) 1)
  (is (depth (Branch (Leaf 4) (Leaf 5))) 2)
  (is (depth (Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3))) 3)
  (is (depth (Branch (Branch Empty Empty) (Leaf 1))) 2)
  )

(finalize)
