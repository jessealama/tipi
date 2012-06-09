;;; terms.lisp A representation of simple terms

(in-package :tipi)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass term () nil)

(defun term? (thing)
  (typep thing 'term))

(defclass function-term (term)
  ((function-symbol :initarg :function
		    :accessor function-symbol
		    :type string)
   (args :initarg :args
	 :accessor arguments
	 :type list)))

(defun make-function-term (function &rest args)
  (make-instance 'function-term
		 :function function
		 :args args))

(defclass variable (term)
  ((name :initarg :name
	 :accessor variable-name
	 :type string)))

(defun variable? (thing)
  (typep thing 'variable))

;;; terms.lisp ends here
