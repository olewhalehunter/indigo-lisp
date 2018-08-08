;; Indigo Common Lisp
;; Algebraic Data Types, Pattern Matching, Strong Static Typing

(defun c+ (&rest strs)
  (apply 'concatenate
	 (append '(string)
		 (mapcar (lambda (x)
			   (if (symbolp x) (symbol-name x) x))
			 strs))))

(defun s+ (&rest symbols)
  (intern (apply 'c+ symbols)))

(defun type-list-format (x)  
  (if (and (listp x) (= (length x) 1))
	  (first x)
	  x))

(defun compose-format (flat-list)
  "'(a b c) -> '((a b) c), 'a -> 'a"
  (if (listp flat-list)
      (if (> (length flat-list) 2)
	  (append (list (compose-format (butlast flat-list)))
		  (last flat-list))
	  (if (= (length flat-list) 2)
	      (subseq flat-list 0 2)
	      flat-list))
      flat-list))

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


(defmacro data (type &rest constructors)
  `(progn
       (setq subtypes
	     (loop for c in (quote ,constructors) collect
		  (case (type-of c)
		    ('symbol
		     (let* ((name c)
			    (supertype (quote ,type))
			    (type-cell `(*type* ,name ,supertype)))
		       (eval
			`(progn
			   (deftype ,name ()
			     '(satisfies ,(s+ 'type- name 'p)))
			   (defun ,(s+ 'type- name 'p) (x)
			     (equal x (quote ,type-cell)))))
		       (setf (get '*type-signatures* name) supertype)
		       (set name type-cell))
		     c)
		    ('cons
		     (let* ((field-name (first c))
			    (field-args (rest c))
			    (field-typep (s+ 'type- field-name 'p))
			    (supertype (quote ,type))
			    (arg-ids (loop
					for arg in field-args
					for n from 0 to (length field-args)
					collect (s+ arg '- n))))
		       (eval
			`(progn
			   (declaim (ftype (function (,@field-args) ,supertype)
					   ,field-name))
			   (defun ,field-name ,arg-ids
			     (list ,@arg-ids))
			   (defun ,field-typep (x)
			     (and x
				  (listp x)
				  (= (length x) (length (quote ,field-args)))
				  ,@(loop
				       for a in field-args
				       for n from 0 to (length field-args)
				       collect
					 `(typep (nth ,n x) (quote ,a))
					 )))
			   (deftype ,field-name ()
			     '(satisfies ,field-typep))			   
			   ))
		       (set-type-signature field-name
					   (append field-args (list (quote ,type))))
		       field-name))
		    )))
       (deftype ,type () `(or ,@subtypes))
       ))


(defmacro check-type-s (x typespec)
  "equivalent of CHECK-TYPE macro but with TYPESPEC as SYMBOL"
  `(assert (typep ,x ,typespec) (x)
	   'type-error :datum ,x :expected-type ,typespec))

(defun check-type-list (cell type)  
  (if (not (endp cell))
      (or (check-type-s (car cell) type)
	  (check-type-list (cdr cell) type))))


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
       ,@(loop for i below (length patterns)
	    for left = (nth i patterns)
	    for right = (nth (incf i) patterns)
	    for type = (if (listp left)
			   (first left)
			   (type-of left))
	    collect
	      (if (eq left 'null)
		  `(null ,right)
		  (if (listp left)
		      `(,type (pattern-rewrite (quote ,left)
					       (quote ,right)
					       args))
		      `(,left ,right)))
	      ))))


(defun function-typespec (name)
  ;; needs portability (function-information ...)
  (sb-kernel:type-specifier (sb-c::info :function :type name)))

(defmacro def-valuetype (name supertype)
  `(let ((type-cell '(*type* ,name ,supertype)))
     (deftype ,name () '(satisfies ,(s+ 'type- name 'p)))
     (defun ,(s+ 'type- name 'p) (x) (equal x type-cell))
     (setf (get '*type-signatures* (quote ,name)) (quote ,supertype))
     (setq ,name type-cell)))

(defun type-of-valuetype (valuetype)
  (third valuetype))

(defun set-type-signature (name signature)
  (setf (get '*type-signatures* name) signature))

(defun type-signature (function)
  (cond ((symbolp function) (get '*type-signatures* function))
	((listp function)
	 (let* ((function (compose-format function))
		(root-function (first function))
		(root-signature (type-signature root-function))
		(args (rest function)))
	   (if (check-type-s (eval (first args)) (first root-signature))
	       (let ((type-signature 
		      (type-list-format (remove (first root-signature)
						root-signature :count 1))))
		 type-signature)
	       )))
	(t (error "undefined TYPE-SIGNATURE"))))
