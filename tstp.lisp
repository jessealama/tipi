
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
	      (let ((annotation (annotations formula)))
		(let ((source (source annotation)))
		  (let ((formula-atoms (flatten-tptp source)))
		    (let ((referring-atoms (remove-if-not #'(lambda (atom)
							     (formula-with-name solution atom))
							  formula-atoms)))
		      (setf sensible
			    (append sensible (select-sensible-formulas referring-atoms solution)))))))))))
    (remove-duplicates sensible :test #'string= :key #'(lambda (x) (format nil "~a" x)))))

(defun restrict-annotation-to-problem-language (annotation solution)
  (let ((source (source annotation)))
    (let ((atoms (flatten-tptp source)))
      (let ((referring-atoms (remove-if-not #'(lambda (atom)
						(formula-with-name solution atom))
					    atoms)))
	(make-instance 'annotation
		       :optional-info nil
		       :source (select-sensible-formulas referring-atoms solution))))))

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
	 (with-slots (name role formula annotations)
	     x
	   (let ((new-formula
		  (make-instance (class-of x)
				 :name name
				 :role role
				 :formula formula
				 :annotations (restrict-annotation-to-problem-language annotations tstp))))
	   (push new-formula filtered-formulas))))
     finally
       (return (make-instance 'tptp-db
			      :formulas (reverse filtered-formulas)))))

(defgeneric subproof-terminating-at (tstp step)
  (:documentation "The subproof of TSTP that terminates at STEP."))

(defmethod subproof-terminating-at (tstp (step integer))
  (subproof-terminating-at tstp (format nil "~d" step)))

(defmethod subproof-terminating-at :around ((tstp tstp-db) (step string))
  (if (formula-with-name tstp step)
      (call-next-method)
      (error "No such formula '~a' in the given TSTP database." step)))

(defmethod subproof-terminating-at ((tstp tstp-db) (step string))
  (let ((formulas (formulas tstp))
	(q (make-instance 'q))
	(supporting-formula-table (make-hash-table :test #'equal)))
    (enqueue-at-front q (list (formula-with-name tstp step)))
    (loop
       :until (empty-queue? q)
       :do
       (let ((formula (remove-front q)))
	 (let ((formula-name (name formula)))
	   (unless (gethash formula-name supporting-formula-table)
	     (when (slot-boundp formula 'source)
	       (let ((source (source formula)))
		 (let ((atoms (flatten source)))
		   (loop
		      :for atom in atoms
		      :for atom-string = (format nil "~a" atom)
		      :when (formula-with-name tstp atom-string)
		      :do
		      (enqueue-at-end q (list (formula-with-name tstp atom-string))))))))
	   (setf (gethash formula-name supporting-formula-table) t))))
    (let ((supporting-formulas (hash-table-keys supporting-formula-table)))
      (let ((sorted-supporting (sort supporting-formulas
				     #'(lambda (formula-1 formula-2)
					 (< (position formula-1 formulas :key #'name :test #'string=)
					    (position formula-2 formulas :key #'name :test #'string=))))))
	(make-instance 'tstp-db
		       :formulas (mapcar #'(lambda (name)
					     (formula-with-name tstp name))
					 sorted-supporting)
		       :problem (problem tstp))))))
