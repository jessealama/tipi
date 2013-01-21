
(in-package :tipi)

(defclass tstp-db (tptp-db)
  ((problem
    :type tptp-db
    :accessor problem
    :initarg :problem)))

(defgeneric parse-tstp (tptp-thing source-problem))

(defmethod parse-tstp :around ((tptp-thing pathname) source-problem)
  (if (probe-file tptp-thing)
      (call-next-method)
      (error "There is no file at '~a'." (namestring tptp-thing))))

(defmethod parse-tstp ((tstp-file pathname) source-problem)
  (make-instance 'tstp-db
		 :formulas (formulas (parse-tptp tstp-file))
		 :problem (parse-tptp source-problem)))

(defmethod parse-tstp ((tstp-string string) source-problem)
  (make-instance 'tstp-db
		 :formulas (formulas (parse-tptp tstp-string))
		 :problem (parse-tptp source-problem)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filtering solutions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric restrict-solution-to-problem-language (tstp)
  (:documentation "Restrict TSTP to the language employed by its underlying problem.  The main application is to eexclude Skolem functions and splitting predicates that are present in the solution but not in the problem."))

(defmethod restrict-solution-to-problem-language ((tstp tstp-db))
  (loop
     with problem = (problem tstp)
     with problem-signature = (signature problem)
     for formula in (formulas tstp)
     when (belongs-to-signature formula problem-signature)
       collect formula into filtered-formulas
     finally
       (return (make-instance 'tptp-db
			      :formulas filtered-formulas))))


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
