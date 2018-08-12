;; Type Inference from Unification with GAMBOL (Prolog) WIP

(in-package :indigo)

(defun gambol-example ()
  (*- (animal ?x) (mammal ?x))
  (*- (mammal dog))
  (*- (mammal cat))

  (pl-solve-all '((animal ?name)))
  )
