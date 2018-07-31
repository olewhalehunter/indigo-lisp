;; Indigo Common Lisp
;; Algebraic Data Types, Pattern Matching, Strong Static Typing

(defun subst-cons-lambda (new-lambda old-lambda cell)
  (if (and cell (listp cell))
      (if (funcall old-lambda cell)
	  (funcall new-lambda cell)
	  (cons (if (listp (car cell))
		    (subst-cons-lambda new-lambda old-lambda (car cell))
		    (if (funcall old-lambda (car cell))
			(funcall new-lambda (car cell))
			(car cell)))
		(subst-cons-lambda new-lambda old-lambda (cdr cell))))))


;;;;;;;;;;;;;;;;;;;;;;;
;; ADT work-in-progress and testing

(defmacro data (name &rest fields)
  `(defstruct ,name
     ,@(loop for field in fields collect
	    (d-bind (name type)
		    field		    
		    `(,name nil :type
			    ,(if (listp type)
				 'cons
				 type))))))

(defmacro check-type-s (x typespec)
  "equivalent of CHECK-TYPE macro but with TYPESPEC as SYMBOL"
  `(assert (typep ,x ,typespec) (x)
	   'type-error :datum ,x :expected-type ,typespec))

(defun check-type-list (cell type)  
  (if (not (endp cell))
      (or (check-type-s (car cell) type)
	  (check-type-list (cdr cell) type))))

(defun nilcadrp (x) (null (cadr x)))


;; end ADT work-in progress
;;;;;;;;;;;;;;;;;;;;;;;

(defun pattern-rewrite (left right input)
  (if (atom right)
      right
      (let* ((args (rest left))	 
	     (right-rewrite
	      (subst-cons-lambda (lambda (x) (list '_rewrite_ x))
				 (lambda (x) (member x args))
				 right)))
	(eval (subst-cons-lambda
	       (lambda (x)
		 (let ((value (nth (position (second x) args) input)))
		   (if (listp value) `(quote ,value) value)))
	       (lambda (x)
		 (and (listp x)
		      (eq (first x) '_rewrite_)))
	       right-rewrite)))))

(defmacro def (name &rest patterns)
  `(defun ,name (args)
     (typecase args
       ,@(loop for p in patterns collect
	      (let* ((left (first p))
		     (right (second p))
		     (type (if (listp left)
			       (first left)
			       (type-of left))))
		(if (eq left 'null)
		    `(null ,right)
		    (if (listp left)
			`(,type (pattern-rewrite (quote ,left)
						 (quote ,right)
						 args))
			`(,left ,right)))
		)))))
