
(in-package :tipi)

(defclass tstp-db (tptp-db)
  ((problem
    :type tptp-db
    :accessor problem
    :initarg :problem)
   (signature-restricted-p
    :type boolean
    :reader solution-restricted-p
    :initarg :restricted
    :initform nil)))

(defgeneric parse-tstp (tptp-thing source-problem))

(defmethod parse-tstp :around ((tptp-thing pathname) source-problem)
  (if (probe-file tptp-thing)
      (call-next-method)
      (error "There is no file at '~a'." (namestring tptp-thing))))

(defmethod parse-tstp ((tstp-file pathname) source-problem)
  (make-instance 'tstp-db
		 :formulas (formulas (parse-tptp tstp-file))
		 :path tstp-file
		 :problem (parse-tptp source-problem)))

(defmethod parse-tstp ((tstp-string string) source-problem)
  (make-instance 'tstp-db
		 :formulas (formulas (parse-tptp tstp-string))
		 :problem (parse-tptp source-problem)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filtering solutions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun select-sensible-formulas (atom-list solution)
  (let* ((sensible nil)
	 (problem (problem solution))
	 (problem-signature (signature problem)))
    (dolist (atom atom-list)
      (let ((formula (formula-with-name solution atom)))
	(when formula
	  (if (belongs-to-signature formula problem-signature)
	      (push atom sensible)
	      (when (slot-boundp formula 'source)
		(let ((source (source formula)))
		  (let ((formula-atoms (flatten-tptp source)))
		    (let ((referring-atoms (remove-if-not #'(lambda (atom)
							      (formula-with-name solution atom))
							  formula-atoms)))
		      (setf sensible
			    (append sensible (select-sensible-formulas referring-atoms solution)))))))))))
    (remove-duplicates sensible :test #'string= :key #'(lambda (x) (format nil "~a" x)))))

(defun restrict-source-to-problem-language (source solution)
  (let ((premises (premises source)))
    (let ((referring-atoms (remove-if-not #'(lambda (atom)
					      (formula-with-name solution atom))
					  premises)))
      (make-instance 'general-list
		     :terms (select-sensible-formulas referring-atoms solution)))))

(defgeneric restrict-solution-to-problem-language (tstp)
  (:documentation "Restrict TSTP to the language employed by its underlying problem.  The main application is to eexclude Skolem functions and splitting predicates that are present in the solution but not in the problem."))

(defmethod restrict-solution-to-problem-language ((tptp tptp-db))
  tptp)

(defmethod restrict-solution-to-problem-language ((tstp tstp-db))
  (loop
     with filtered-formulas = nil
     with problem = (expand-includes (problem tstp))
     with problem-signature = (signature problem)
     for x in (formulas tstp)
     do
       (when (belongs-to-signature x problem-signature)
	 (with-slots (name role formula)
	     x
	   (let ((new-formula
		  (make-instance (class-of x)
				 :name name
				 :role role
				 :formula formula)))
	     (when (slot-boundp x 'source)
	       (setf (source new-formula)
		     (restrict-source-to-problem-language (source x) tstp)))
	     (when (slot-boundp x 'optional-info)
	       (setf (optional-info new-formula)
		     (optional-info x)))
	   (push new-formula filtered-formulas))))
     finally
       (return (make-instance 'tstp-db
			      :formulas (reverse filtered-formulas)
			      :problem problem
			      :restricted t))))

(defmethod subproof-terminating-at ((tstp tstp-db) step)
  (make-instance 'tstp-db
		 :restricted (solution-restricted-p tstp)
		 :path (path tstp)
		 :problem (problem tstp)
		 :formulas (formulas (subproof-terminating-at (make-instance 'tptp-db
									     :formulas (formulas tstp))
							      step))))

(defmethod kowalski ((db tstp-db))
  (make-instance 'tstp-db
		 :restricted (solution-restricted-p db)
		 :path (path db)
		 :problem (problem db)
		 :formulas (mapcar #'kowalski
				   (formulas db))))

(defmethod squeeze-quantifiers ((db tstp-db))
  (make-instance 'tstp-db
		 :restricted (solution-restricted-p db)
		 :path (path db)
		 :problem (problem db)
		 :formulas (mapcar #'squeeze-quantifiers (formulas db))))

(defmethod supporting-axioms :around ((db tstp-db))
  (let ((new-db (call-next-method)))
    (make-instance 'tstp-db
		   :restricted (solution-restricted-p db)
		   :path (path db)
		   :problem (problem db)
		   :formulas (formulas new-db))))

(defmethod reduce-trivial-equivalences ((db tstp-db))
  (make-instance 'tstp-db
		 :path (path db)
		 :restricted (solution-restricted-p db)
		 :problem (problem db)
		 :formulas (reduce-trivial-equivalences (formulas db))))

(defmethod reduce-equivalences ((db tstp-db) premises &key predicate)
  (make-instance 'tstp-db
		 :path (path db)
		 :restricted (solution-restricted-p db)
		 :problem (problem db)
		 :formulas (reduce-equivalences (formulas db)
						premises
						:predicate predicate)))

(defmethod rename-symbol ((db tstp-db) old-name new-name)
  (make-instance 'tstp-db
		 :formulas (mapcar #'(lambda (x)
				       (rename-symbol x old-name new-name))
				   (formulas db))
		 :restricted (solution-restricted-p db)
		 :problem (rename-symbol (problem db) old-name new-name)
		 :path (path db)))

(defmethod fofify ((db tstp-db))
  (make-instance 'tstp-db
		 :path (path db)
		 :formulas (mapcar #'fofify (formulas db))
		 :restricted (solution-restricted-p db)
		 :problem (problem db)))

(defun topological-sort (tstp)
  (let ((dep-table (dependency-table tstp)))
    (flet ((less-than (formula-1 formula-2)
	     (or (string= (role formula-1) "axiom")
		 (exists-path (stringify (name formula-2))
			      (stringify (name formula-1))
			      dep-table))))
      (let ((sorted (sort (formulas tstp) #'less-than)))
	(make-instance 'tstp-db
		       :path (path tstp)
		       :problem (problem tstp)
		       :formulas sorted
		       :restricted (solution-restricted-p tstp))))))

(defmethod eliminate-truth-values ((db tstp-db))
  (make-instance 'tstp-db
		 :formulas (mapcar #'eliminate-truth-values (formulas db))
		 :problem (problem db)
		 :restricted (solution-restricted-p db)
		 :path (path db)))

(defmethod dependency-table ((db tstp-db))
  (dependency-table (make-instance 'tptp-db
				   :formulas (formulas db))))
