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
		    :type symbol)
   (args :initarg :args
	 :accessor arguments
	 :type list)))

(defmethod print-object ((function function-term) stream)
  (let ((func (function-symbol function))
	(args (arguments function)))
    (if (null args)
	(format stream "~a" func)
	(format stream "~a(~{~a,~})" func args))))

(defun make-function-term (function &rest args)
  (make-instance 'function-term
		 :function function
		 :args args))

(defclass variable (term)
  ((name :initarg :name
	 :accessor variable-name
	 :type string)))

(defmethod print-object ((var variable) stream)
  (format stream "~a" (variable-name var)))

(defun variable? (thing)
  (typep thing 'variable))

(defgeneric form->term (form)
  (:documentation "Attempt to understand FORM as a term."))

(defmethod form->term ((list list))
  (if (null list)
      (error 'parse-form-empty-list-supplied-error)
      (op-and-args->term (symbolify-here (car list))
			 (cdr list))))

(defmethod form->term ((sym symbol))
  (let ((name (symbol-name sym)))
    (if (empty-string? name)
	(error 'parse-form-empty-string-supplied)
	(let ((first-char (char name 0)))
	  (if (char= first-char #\?)
	      (make-instance 'variable
			     :name (subseq name 1))
	      (make-function-term name))))))

;;; terms.lisp ends here
