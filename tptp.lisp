
(in-package :tipi)

(defparameter *tptp-to-lisp-stylesheet*
  #p"/Users/alama/sources/xsl4tptp/tptp-to-lisp.xsl")

(defgeneric xmlize-tptp (tptp))

(defmethod xmlize-tptp :around ((tptp-file pathname))
  (if (probe-file tptp-file)
      (call-next-method)
      (error "There is no file at '~a'." (namestring tptp-file))))

(defmethod xmlize-tptp ((tptp-file pathname))
  (with-output-to-string (stdout)
    (run-program "tptp4X"
		 (list "-c" "-x" "-fxml" "--")
		 :wait t
		 :output stdout
		 :input tptp-file)))

(defgeneric read-tptp (tptp-thing))

(defmethod read-tptp :around ((tptp-thing pathname))
  (if (probe-file tptp-thing)
      (call-next-method)
      (error "There is no file at '~a'." (namestring tptp-thing))))

(defmethod read-tptp ((tptp-file pathname))
  (let ((lisp-string (apply-stylesheet *tptp-to-lisp-stylesheet*
				       (xmlize-tptp tptp-file)
				       nil
				       nil)))
    (with-readtable (find-readtable :modern)
      (let ((tptp-form (handler-case (read-from-string lisp-string)
			 (error (c) (error "Unable to make sense of~%~%~a~%~%as a Lisp representation of~%~%  ~a~%~%The error was:~%~%  ~a" lisp-string (namestring tptp-file) c)))))
	(let ((problem (make-instance 'tptp-problem)))
	  (setf (formulas problem)
		(mapcar #'make-tptp-formula tptp-form))
	  problem)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass tptp-problem ()
  ((formulas :type list
	     :accessor formulas
	     :initform nil)))

(defmethod print-object ((problem tptp-problem) stream)
  (print-unreadable-object
      (problem stream :type t :identity t)
    (let ((formulas (formulas problem)))
      (if formulas
	  (format stream "~{~a~%~}" formulas)
	  (format stream "(empty list of formulas/clauses)")))))

(defun has-conjecture-formula? (problem)
  (first (member "conjecture"
		 (formulas problem)
		 :key #'status
		 :test #'string=)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass tptp-formula ()
  ((name :type string
	 :initarg :name
	 :accessor name
	 :initform (error "A TPTP formula requires a name."))
   (syntax :type string
	   :initarg :syntax
	   :accessor syntax
	   :initform (error "A TPTP formula requires a syntax."))
   (status :type string
	   :initarg :status
	   :accessor status
	   :initform (error "A TPTP formula requires a status/role."))
   (formula :type formula
	    :initarg :formula
	    :accessor formula
	    :initform (error "A TPTP formula requires a formula proper."))
   (source :type list
	   :accessor source
	   :initarg source)
   (useful-info :type list
		:accessor useful-info
		:initarg useful-info)))

(defmethod print-object ((formula tptp-formula) stream)
  (print-unreadable-object
      (formula stream :identity t :type t)
    (format stream "~a : ~a" (name formula) (formula formula))))

(defgeneric make-tptp-formula (thing))

(defmethod make-tptp-formula ((thing list))
  (destructuring-bind (syntax name status formula . more-stuff)
      thing
    (declare (ignore more-stuff))
    (make-instance 'tptp-formula
		   :name (symbol-name name)
		   :syntax (symbol-name syntax)
		   :status (symbol-name status)
		   :formula (form->formula formula))))
