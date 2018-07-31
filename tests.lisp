;;;;;;;;;;;;;;;;;;;;;;;;
;; currently bundling ADT definition into a macro
(declaim (ftype (function (integer) tree) leaf))
(defun Leaf (x) (cons x nil))

(declaim (ftype (function (tree tree) tree) branch))
(defun Branch (x y) (list x y))

(defun branchp (x) (and x
 			(listp x)
			(typep (car x) 'tree)
			(typep (cadr x) 'tree)))

(deftype Tree () '(or null leaf branch))

(deftype Leaf () '(and (cons integer) (satisfies nilcadrp)))

(deftype Branch () '(and (satisfies branchp)))
;;;;;;;;;;;;;;;;;;;;;;;;

(def depth
  ( null 0)
  ( (Leaf n) 1 )
  ( (Branch l r) (+ 1 (max (depth l)
			   (depth r)))))


(depth nil)
(depth (Leaf 7))
(depth (Branch nil nil))
(depth (Branch (Leaf 4) (Leaf 5)))
(depth (Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3)))
(depth (Branch (Branch nil nil) (Leaf 1)))
