(data Tree
      Empty
      (Leaf Integer)
      (Branch Tree Tree))

(def depth
  ( Empty 0)
  ( (Leaf n) 1 )
  ( (Branch l r) (+ 1 (max (depth l)
			   (depth r)))))



(typep Empty 'Tree)
(typep (Leaf 3) 'Tree)
(typep (Branch (Leaf 2) (Leaf 4)) 'Tree)
(typep (Branch Empty Empty) 'Tree)


(depth Empty)
(depth (Leaf 7))
(depth (Branch Empty Empty))
(depth (Branch (Leaf 4) (Leaf 5)))
(depth (Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3)))
(depth (Branch (Branch Empty Empty) (Leaf 1)))
