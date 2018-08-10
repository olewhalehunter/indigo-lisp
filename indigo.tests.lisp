(in-package :indigo.tests)

(defun run-tests ()
  (if (prove:run #P"test/tree.lisp" :reporter :tap) t nil))
