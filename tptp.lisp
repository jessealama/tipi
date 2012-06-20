
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
	(let ((problem (make-instance 'tptp-db)))
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
;; TPTP databases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass tptp-db ()
  ((formulas :type list
	     :initarg :formulas
	     :accessor formulas
	     :initform nil)
   (path
    :type pathname
    :accessor path)))

(defmethod print-object ((problem tptp-db) stream)
  (print-unreadable-object
      (problem stream :type t :identity nil)
    (let ((formulas (formulas problem)))
      (if formulas
	  (format stream "~{~a~%~}" formulas)
	  (format stream "(empty list of formulas/clauses)")))))

(defclass derivability-problem (tptp-db)
  ((conjecture
    :initarg :conjecture
    :accessor conjecture
    :initform (error "To specify a derivability problem, a conjecture must be supplied."))))

(defmethod print-object ((problem derivability-problem) stream)
  (print-unreadable-object (problem stream :type t :identity nil)
    (let ((conjecture (conjecture problem))
	  (formulas (formulas problem)))
      (format stream "Conjecture: ~a" conjecture)
      (if formulas
	  (format stream "Premises:~%~{~a~%~}" formulas)
	  (format stream "Premises: (none)")))))

(defgeneric make-derivability-problem (formulas))

(defmethod make-derivability-problem ((formulas tptp-db))
  (let ((conjecture (conjecture-formula formulas)))
    (if conjecture
	(make-instance 'derivability-problem
		       :formulas (non-conjecture-formulas formulas)
		       :conjecture conjecture)
	(error "There is no conjecture formula in ~a." formulas))))

(defmethod render ((problem tptp-db))
  (format nil "~{~a~%~}" (mapcar #'render (formulas problem))))

(defgeneric proper-formulas (problem))

(defmethod proper-formulas ((problem tptp-db))
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
  (make-instance 'tptp-db
		 :formulas (remove-if #'conjecture-string?
				      (formulas problem)
				      :key #'status)))

(defgeneric remove-formula (formulas formula))

(defmethod remove-formula ((formulas tptp-db) (formula-name string))
  "Remove any formula in FORMULAS whose name is FORMULA-NAME."
  (make-instance 'tptp-db
		 :formulas (remove-if #'(lambda (x) (string= x formula-name))
				      (formulas formulas)
				      :key #'name)))

(defmethod remove-formula ((formulas tptp-db) (formula tptp-formula))
  (remove-formula formulas (name formula)))

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
	(make-instance 'tptp-db
		       :formulas (cons (change-status conjecture "axiom")
				       (non-conjecture-formulas problem)))
	problem)))

(defun formula-names (tptp-db)
  (mapcar #'name (formulas tptp-db)))

(defgeneric formula-with-name (tptp-db name))

(defmethod formula-with-name ((tptp-db tptp-db) (name symbol))
  (formula-with-name tptp-db (symbol-name name)))

(defmethod formula-with-name ((tptp-db tptp-db) (name string))
  (first (remove-if-not #'(lambda (x) (string= x name))
			(formulas tptp-db)
			:key #'name)))

(defgeneric premises (problem))

(defmethod premises ((db tptp-db))
  (non-conjecture-formulas db))
