
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
		(mapcar #'make-tptp-formula tptp-form)
		(path problem)
		tptp-file)
	  problem)))))

(defmethod read-tptp ((tptp-string string))
  (let ((temp (temporary-file)))
    (with-open-file (tptp-file temp
			       :direction :output
			       :if-does-not-exist :create
			       :if-exists :supersede)
      (format tptp-file "~a" tptp-string))
    (read-tptp temp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass tptp-problem ()
  ((formulas :type list
	     :initarg :formulas
	     :accessor formulas
	     :initform nil)
   (path
    :type pathname
    :accessor path)))

(defmethod print-object ((problem tptp-problem) stream)
  (print-unreadable-object
      (problem stream :type t :identity nil)
    (let ((formulas (formulas problem)))
      (if formulas
	  (format stream "~{~a~%~}" formulas)
	  (format stream "(empty list of formulas/clauses)")))))

(defmethod render ((problem tptp-problem))
  (format nil "~{~a~%~}" (mapcar #'render (formulas problem))))

(defgeneric proper-formulas (problem))

(defmethod proper-formulas ((problem tptp-problem))
  (mapcar #'formula (formulas problem)))

(defun conjecture-formula (problem)
  (has-conjecture-formula? problem))

(defun has-conjecture-formula? (problem)
  (first (member "conjecture"
		 (formulas problem)
		 :key #'status
		 :test #'string=)))

(defun conjecture-string? (string)
  (string= string "conjecture"))

(defun remove-conjecture (problem)
  (make-instance 'tptp-problem
		 :formulas (remove-if #'conjecture-string?
				      (formulas problem)
				      :key #'status)))

(defun formulas-with-status (problem status)
  (remove-if-not #'(lambda (stat) (string= stat status))
		 (formulas problem)
		 :key #'status))

(defun statuses-of-formulas (problem)
  (loop
     with statuses = (make-hash-table :test #'equal)
     for formula in (formulas problem)
     for status = (status formula)
     do (setf (gethash status statuses) 0)
     finally (return (hash-table-keys statuses))))

(defun non-conjecture-formulas (problem)
  (remove-if #'(lambda (stat) (string= stat "conjecture"))
	     (formulas problem)
	     :key #'status))

(defun change-status (formula new-status)
  (make-instance 'tptp-formula
		 :name (name formula)
		 :syntax (syntax formula)
		 :status new-status
		 :formula (formula formula)
		 :source (when (slot-boundp formula 'source)
			   (source formula))
		 :useful-info (when (slot-boundp formula 'useful-info)
				(useful-info formula))))

(defun promote-conjecture-to-axiom (problem)
  (let ((conjecture (has-conjecture-formula? problem)))
    (if conjecture
	(make-instance 'tptp-problem
		       :formulas (cons (change-status conjecture "axiom")
				       (non-conjecture-formulas problem)))
	problem)))

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
   (source
    :accessor source
    :initarg :source)
   (useful-info :type list
		:accessor useful-info
		:initarg :useful-info)))

(defmethod print-object ((formula tptp-formula) stream)
  (print-unreadable-object
      (formula stream :identity nil :type t)
    (if (slot-boundp formula 'source)
	(format stream "~a (~a): ~a [source: ~a]" (name formula) (status formula) (formula formula) (source formula))
	(format stream "~a (~a): ~a" (name formula) (status formula) (formula formula)))))

(defun render-syntax (formula)
  (let ((syntax (syntax formula)))
    (cond ((string= syntax "formula") "fof")
	  ((string= syntax "clause") "cnf")
	  (t
	   (error "Don't know how to render formulas whose syntax is '~a'." syntax)))))

(defmethod render ((formula tptp-formula))
  (format nil "~a(~a,~a,~a)."
	  (render-syntax formula)
	  (name formula)
	  (status formula)
	  (formula formula)))

(defgeneric make-tptp-formula (thing))

(defmethod make-tptp-formula ((thing list))
  (destructuring-bind (syntax name status formula . more-stuff)
      thing
    (if more-stuff
	(destructuring-bind (source . useful-info)
	    more-stuff
	  (make-instance 'tptp-formula
		   :name (if (symbolp name)
			     (symbol-name name)
			     (format nil "~a" name))
		   :syntax (symbol-name syntax)
		   :status (symbol-name status)
		   :formula (form->formula formula)
		   :source source
		   :useful-info useful-info))
	(make-instance 'tptp-formula
		   :name (if (symbolp name)
			     (symbol-name name)
			     (format nil "~a" name))
		   :syntax (symbol-name syntax)
		   :status (symbol-name status)
		   :formula (form->formula formula)))))

(defgeneric used-premises (solution premise-pool))

(defmethod used-premises ((solution tptp-problem) (background-theory tptp-problem))
  (let ((background-premises (mapcar #'name (formulas background-theory)))
	(used-table (make-hash-table :test #'equal)))
    (loop
       for solution-formula in (formulas solution)
       do
	 (when (slot-boundp solution-formula 'source)
	   (let ((source (source solution-formula)))
	     (let ((used-names (atoms-in-list source background-premises)))
	       (loop
		  for used in used-names
		  do (setf (gethash used used-table) 0))))))
    (hash-table-keys used-table)))
