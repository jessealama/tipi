
(in-package :tipi)

(defclass first-order-signature ()
  ((functions
    :type list
    :initform nil
    :initarg :functions
    :accessor functions)
   (predicates
    :type list
    :initform nil
    :initarg :predicates
    :accessor predicates)))

(defparameter *empty-sigature*
  (make-instance 'first-order-signature))

(defclass function-symbol ()
  ((arity
    :type integer
    :accessor arity
    :initform (error "An arity is required to create a function symbol.")
    :initarg :arity)
   (name
    :type symbol
    :accessor name
    :initform (error "A name is required to create a function symbol.")
    :initarg :name)))

(defmethod print-object ((object function-symbol) stream)
  (let ((name (name object))
	(arity (arity object)))
    (print-unreadable-object (object stream :type t :identity nil)
      (format stream "~a (arity ~d)" name arity))))

(defclass predicate-symbol ()
  ((arity
    :type integer
    :accessor arity
    :initform (error "An arity is required to create a predicate symbol.")
    :initarg :arity)
   (name
    :type symbol
    :accessor name
    :initform (error "A name is required to create a predicate symbol.")
    :initarg :name)))

(defparameter *constant-true-symbol*
  (make-instance 'predicate-symbol
		 :arity 0
		 :name :|true|))

(defparameter *constant-false-symbol*
  (make-instance 'predicate-symbol
		 :arity 0
		 :name :|false|))

(defparameter *equality-symbol*
  (make-instance 'predicate-symbol
		 :arity 2
		 :name :|=|))

(defmethod print-object ((object predicate-symbol) stream)
  (let ((name (name object))
	(arity (arity object)))
    (print-unreadable-object (object stream :type t :identity nil)
      (format stream "~a (arity ~d)" name arity))))

(defmethod print-object ((object first-order-signature) stream)
  (let ((preds (predicates object))
	(funcs (functions object)))
    (print-unreadable-object (object stream :type t :identity nil)
      (cond ((and (null preds)
		  (null funcs))
	     (format stream "(empty)"))
	    ((null preds)
	     (format stream "0 predicate symbols; ~d function symbol(s):~%~{~a~%~}" (length funcs) funcs))
	    ((null funcs)
	     (format stream "0 function symbols; ~d predicate symbol(s):~%~{~a~%~}" (length preds) preds))
	    (t
	     (format stream "~d function symbol(s):~%~{~a~%~}~%~d predicate symbols:~%~{~a~%~}" (length funcs) funcs (length preds) preds))))))

(defun merge-signatures (signature-1 signature-2)
  (let ((preds-1 (predicates signature-1))
	(preds-2 (predicates signature-2))
	(funcs-1 (functions signature-1))
	(funcs-2 (functions signature-2)))
    (let ((preds (remove-duplicates (append preds-1 preds-2)
				    :test #'equal-predicate-symbol-p))
	  (funcs (remove-duplicates (append funcs-1 funcs-2)
				    :test #'equal-function-symbol-p)))
      (make-instance 'first-order-signature
		     :predicates preds
		     :functions funcs))))

(defun equal-predicate-symbol-p (pred-symbol-1 pred-symbol-2)
  (and (string= (name pred-symbol-1)
		(name pred-symbol-2))
       (= (arity pred-symbol-1)
	  (arity pred-symbol-2))))

(defun equal-function-symbol-p (func-symbol-1 func-symbol-2)
  (and (string= (name func-symbol-1)
		(name func-symbol-2))
       (= (arity func-symbol-1)
	  (arity func-symbol-2))))

(defgeneric signature (tptp)
  (:documentation "The signature of TPTP."))

(defmethod signature ((formula atomic-formula))
  (let ((arguments (arguments formula)))
    (let ((pred-symbol (make-instance 'predicate-symbol
				      :name (predicate formula)
				      :arity (length arguments))))
      (let ((sig (make-instance 'first-order-signature
				:predicates (list pred-symbol))))
	(reduce #'merge-signatures
	      (cons sig (mapcar #'signature arguments)))))))

(defmethod signature ((formula unary-connective-formula))
  (signature (argument formula)))

(defmethod signature ((formula binary-connective-formula))
  (merge-signatures (signature (lhs formula))
		    (signature (rhs formula))))

(defmethod signature ((formula generalization))
  (signature (matrix formula)))

(defmethod signature ((term variable-term))
  *empty-sigature*)

(defmethod signature ((term function-term))
  (let ((arguments (arguments term)))
    (let ((func-symbol (make-instance 'function-symbol
				      :name (function-symbol term)
				      :arity (length arguments))))
      (let ((sig (make-instance 'first-order-signature
				:functions (list func-symbol))))
	(reduce #'merge-signatures
	      (cons sig (mapcar #'signature arguments)))))))

(defun subsignature-p (signature-1 signature-2)
  (let ((preds-1 (predicates signature-1))
	(funcs-1 (functions signature-1))
	(preds-2 (predicates signature-2))
	(funcs-2 (functions signature-2)))
    (pushnew *constant-true-symbol* preds-2 :test #'equal-predicate-symbol-p)
    (pushnew *constant-false-symbol* preds-2 :test #'equal-predicate-symbol-p)
    (pushnew *equality-symbol* preds-2 :test #'equal-predicate-symbol-p)
    (and (subsetp preds-1 preds-2 :test #'equal-predicate-symbol-p)
	 (subsetp funcs-1 funcs-2 :test #'equal-function-symbol-p))))

(defun belongs-to-signature (formula signature)
  (subsignature-p (signature formula) signature))
