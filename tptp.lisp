
(in-package :tipi)

(defparameter *tptp-to-lisp-stylesheet*
  #p"/Users/alama/sources/xsl4tptp/tptp-to-lisp.xsl")

(defgeneric xmlize-tptp (tptp))

(defmethod xmlize-tptp :around ((tptp-file pathname))
  (if (probe-file tptp-file)
      (call-next-method)
      (error "There is no file at '~a'." (namestring tptp-file))))

(defmethod xmlize-tptp ((tptp-file pathname))
  (let ((tptp4X-out (make-string-output-stream))
	(tptp4X-err (make-string-output-stream))
	(tptp-dir (pathname (directory-namestring tptp-file))))
    (with-current-directory (tptp-dir)
      (run-program "tptp4X"
		   (list "-c" "-x" "-fxml" "--")
		   :wait t
		   :output tptp4X-out
		   :error tptp4X-err
		   :input tptp-file))
    (prog1
	(get-output-stream-string tptp4X-out)
      (close tptp4X-out)
      (close tptp4X-err))))

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
    :accessor path
    :initarg :path)))

(defmethod initialize-instance :after ((db tptp-db) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (if (slot-boundp db 'path)
      (let ((path (path db)))
	(if (file-exists-p path)
	    db
	    (error "No such file '~a'." path)))
      db))

(defmethod print-object ((problem tptp-db) stream)
  (print-unreadable-object
      (problem stream :type t :identity nil)
    (let ((formulas (formulas problem)))
      (if formulas
	  (format stream "~{~a~%~}" formulas)
	  (format stream "(empty list of formulas/clauses)")))))

(defmethod signature ((tptp tptp-db))
  (reduce #'merge-signatures (mapcar #'signature
				     (mapcar #'formula (formulas tptp)))))

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

(defmethod initialize-instance :after ((problem derivability-problem) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (when (some #'(lambda (premise)
		  (string= (status premise) "conjecture"))
	      (formulas problem))
    (error "Some non-conjecture formula has the TPTP status 'conjecture'."))
  (let ((conjecture (conjecture problem)))
    (unless (string= (status conjecture) "conjecture")
      (setf (conjecture problem) (change-status conjecture "conjecture"))))
  problem)

(defgeneric make-derivability-problem (formulas))

(defmethod make-derivability-problem ((formulas tptp-db))
  (let ((conjecture (conjecture-formula formulas)))
    (if conjecture
	(make-instance 'derivability-problem
		       :formulas (non-conjecture-formulas formulas)
		       :conjecture conjecture)
	(error "There is no conjecture formula in ~a." formulas))))

(defmethod make-derivability-problem ((formulas null))
  (error "The empty list does not contain a conjecture formula."))

(defmethod make-derivability-problem ((formulas list))
  (let ((conjecture (find "conjecture" formulas :test #'string= :key #'status))
	(non-conjecture-formulas (remove-if #'(lambda (formula)
						(string= (status formula) "conjecture"))
					    formulas)))
    (if conjecture
	(make-instance 'derivability-problem
		       :formulas non-conjecture-formulas
		       :conjecture conjecture)
	(error "No conjecture formula found in ~{~a~%~}" formulas))))

(defmethod make-derivability-problem ((problem pathname))
  (make-derivability-problem (parse-tptp problem)))

(defmethod render ((problem tptp-db))
  (format nil "~{~a~%~}" (mapcar #'render (formulas problem))))

(defmethod render ((problem derivability-problem))
  (with-output-to-string (s)
    (dolist (formula (formulas problem))
      (format s "~a" (render formula))
      (terpri s))
    (format s "~a" (render (conjecture problem)))
    (terpri s)))

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

(defmethod remove-formula ((problem derivability-problem) (formula tptp-formula))
  (let ((name-to-remove (name formula))
	(conjecture-name (name (conjecture problem))))
    (if (string= name-to-remove conjecture-name)
	(make-instance 'tptp-db
		       :formulas (formulas problem))
	(make-instance 'derivability-problem
		       :conjecture (conjecture problem)
		       :formulas (remove-if #'(lambda (x) (string= x name-to-remove))
					    (formulas problem)
					    :key #'name)))))

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

(defgeneric change-status-of-formula-in (formula problem new-status)
  (:documentation "Change the TPTP status of FORMULA in PROBLEM to NEW-STATUS."))

(defmethod change-status-of-formula-in ((formula string)
					(problem pathname)
					(new-status string))
  (change-status-of-formula-in formula (parse-tptp problem) new-status))

(defmethod change-status-of-formula-in ((formula string)
					(problem tptp-db)
					(new-status string))
  (let ((formula-in-problem (formula-with-name problem formula)))
    (if formula-in-problem
	(let ((other-formulas (remove-if #'(lambda (name)
					     (string= name formula))
					 (formulas problem)
					 :key #'name)))
	  (let ((new-formula (change-status formula-in-problem new-status)))
	    (make-instance 'tptp-db
			   :formulas (cons new-formula
					   other-formulas)))))))

(defmethod change-status-of-formula-in ((formula string)
					(problem derivability-problem)
					(new-status string))
  (if (string= new-status "conjecture")
      (let ((conjecture (conjecture problem)))
	(let ((conjecture-name (name conjecture)))
	  (if (string= conjecture-name formula)
	      problem
	      (error "The given derivability-problem already has a conjecture formula; (by the name ~a), so we cannot change the status of ~a into 'conjecture'." conjecture-name formula))))
      (let ((formula-in-problem (formula-with-name problem formula)))
    (if formula-in-problem
	(let ((other-formulas (remove-if #'(lambda (name)
					     (string= name formula))
					 (formulas problem)
					 :key #'name)))
	  (let ((new-formula (change-status formula-in-problem new-status)))
	    (make-instance 'tptp-db
			   :formulas (cons new-formula
					   other-formulas))))))))

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

(defgeneric restrict-to (db formulas)
  (:documentation "Restrict DB to the formulas in FORMULAS."))

(defmethod restrict-to ((db tptp-db) (formulas list))
  (let ((new-formulas nil))
    (dolist (formula formulas)
      (cond ((stringp formula)
	     (let ((formula-in-db (formula-with-name db formula)))
	       (when formula-in-db
		 (push formula-in-db new-formulas))))
	    ((typep formula 'formula)
	     (let ((formula-in-db (formula-with-name db (name formula))))
	       (when formula-in-db
		 (push formula-in-db new-formulas))))
	    (t
	     (error "Don't know how to handle ~a." formula))))
    (make-instance 'tptp-db
		   :formulas new-formulas)))

(defmethod restrict-to ((problem derivability-problem) (formulas list))
  (let* ((new-formulas nil)
	 (conjecture (conjecture problem))
	 (conjecture-name (name conjecture)))
    (dolist (formula formulas)
      (cond ((stringp formula)
	     (let ((formula-in-db (formula-with-name problem formula)))
	       (when formula-in-db
		 (unless (string= formula conjecture-name)
		   (push formula-in-db new-formulas)))))
	    ((typep formula 'formula)
	     (let ((formula-in-db (formula-with-name problem (name formula))))
	       (when formula-in-db
		 (unless (string= (name formula) conjecture-name)
		   (push formula-in-db new-formulas)))))
	    (t
	     (error "Don't know how to handle ~a." formula))))
    (make-instance 'derivability-problem
		   :conjecture conjecture
		   :formulas new-formulas)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Includes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass include-instruction ()
  ((file
    :accessor file
    :initarg :file
    :initform (error "An include instruction requires a file name."))
   (selection
    :accessor selection
    :initarg :selection
    :type list
    :initform nil)))
