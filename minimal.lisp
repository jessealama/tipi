
(in-package :tipi)

(defgeneric minimal-solution? (solution conjecture background-premises)
  (:documentation "Is CONJECTURE entailed by a proper subset of the premises used in
  the solution SOLUTION?  (BACKGROUND-THEORY is used to determine
  which formulas of SOLUTION are 'used'."))

(defmethod minimal-solution? ((solution tptp-db)
			     (conjecture tptp-formula)
			     (background-premises tptp-db))
  (null (extraneous-premises solution conjecture background-premises)))

(defmethod minimal-solution? (solution
			      (conjecture-name string)
			      (background-premises tptp-db))
  (let ((formula (formula-with-name background-premises conjecture-name)))
    (if formula
	(minimal-solution? solution formula background-premises)
	(error "There is no formula in ~a with the name '~a'." background-premises conjecture-name))))

(defmethod minimal-solution? ((used-premises list)
			     (conjecture tptp-formula)
			     (background-premises tptp-db))
  (null (extraneous-premises used-premises conjecture background-premises)))

(defgeneric minimize (problem &key skip-initial-proof keep timeout))

(defmethod minimize :around ((problem derivability-problem) &key skip-initial-proof keep timeout)
  (declare (ignore keep))
  (if skip-initial-proof
      (call-next-method)
      (let* ((initial-result (solve *eprover* problem :timeout timeout))
	     (status (szs-status initial-result)))
	(if (szs-implies? status (lookup-szs-status "Theorem"))
	    (let* ((conjecture (conjecture problem))
		   (result-as-db (interpret initial-result))
		   (used-premise-names (used-premises result-as-db problem))
		   (used-premises (mapcar #'(lambda (name) (formula-with-name problem name)) used-premise-names))
		   (trimmed-problem (make-derivability-problem (cons conjecture used-premises)))
		   (needed (needed-premises trimmed-problem)))
	      (minimize trimmed-problem
			:skip-initial-proof t
			:keep needed
			:timeout timeout))
	    (error "The initial problem could not be solved (SZS status ~a)." status)))))

(defmethod minimize ((problem derivability-problem) &key skip-initial-proof keep timeout)
  (declare (ignore skip-initial-proof))
  (let ((conjecture (conjecture problem))
	(premises (formulas problem))
	(solutions nil)
	(unknown nil)
	(non-solutions nil))
    (labels ((already-known (subset)
	       (or (some #'(lambda (solution)
			     (subsetp solution subset :test #'string= :key #'name))
			 solutions)
		   (some #'(lambda (non-solution)
			     (subsetp subset non-solution :test #'string= :key #'name))
			 non-solutions)))
	     (test-subset (subset)
	       (when (subsetp keep subset :test #'string= :key #'name)
		 (if (already-known subset)
		     (progn
		       ;; (format t "Skipping a settled subset.")
		       )
		     (let* ((p (make-derivability-problem (cons conjecture subset)))
			    (szs-status (solve (list *eprover* *paradox*) p :timeout timeout)))
		       (if (is-szs-success? szs-status)
			   (if (szs-implies? szs-status (lookup-szs-status "Theorem"))
			       (progn
				 (push subset solutions)
				 ;; (format t "Solution: ~{~a~%~}" (mapcar #'name subset))
				 )
			       (progn
				 (push subset non-solutions)
				 ;; (format t "Non-solution: ~{~a~%~}" (mapcar #'name subset))
				 ))
			   (progn
			     (push subset unknown)
			     ;; (format t "Unknown: ~{~a~%~}" (mapcar #'name subset))
			     ))))
		 ;; (terpri)
		 )))
      (map-all-combinations #'test-subset premises))
    (list (mapcar #'sort-formula-list solutions)
	  (mapcar #'sort-formula-list non-solutions)
	  (mapcar #'sort-formula-list unknown))))

(defmethod minimize ((problem pathname) &key skip-initial-proof keep timeout)
  (let ((db (read-tptp problem)))
    (let ((conjecture (conjecture-formula db)))
      (if conjecture
	  (let ((p (make-derivability-problem (cons conjecture
						    (non-conjecture-formulas db)))))
	    (minimize p
		      :skip-initial-proof skip-initial-proof
		      :keep keep
		      :timeout timeout))
	  (minimize db
		    :skip-initial-proof skip-initial-proof
		    :keep keep
		    :timeout timeout)))))
