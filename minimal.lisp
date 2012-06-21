
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

(defgeneric minimize (problem &key skip-initial-proof keep))

(defmethod minimize :around ((problem derivability-problem) &key skip-initial-proof keep)
  (declare (ignore keep))
  (if skip-initial-proof
      (call-next-method)
      (let* ((initial-result (solve *eprover* problem))
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
			:keep needed))
	    (error "The initial problem could not be solved (SZS status ~a)." status)))))

(defmethod minimize ((problem derivability-problem) &key skip-initial-proof keep)
  (declare (ignore skip-initial-proof))
  (let ((conjecture (conjecture problem))
	(premises (formulas problem))
	(solutions nil)
	(unknown nil)
	(non-solutions nil))
    (flet ((test-subset (subset)
	     (when (and (subsetp keep subset) (not (some-subset subset solutions)))
	       (let* ((p (make-derivability-problem (cons conjecture subset)))
		      (szs-status (solve (list *eprover* *paradox*) p)))
		 (setf subset (mapcar #'name subset))
		 (setf subset (sort subset #'string<))
		 (if (is-szs-success? szs-status)
		     (if (szs-implies? szs-status (lookup-szs-status "Theorem"))
			 (push subset solutions)
			 (push subset non-solutions))
		     (push subset unknown))))))
      (map-all-combinations #'test-subset premises))
    (values solutions non-solutions unknown)))

(defmethod minimize ((problem pathname) &key skip-initial-proof keep)
  (let ((db (read-tptp problem)))
    (let ((conjecture (conjecture-formula db)))
      (if conjecture
	  (let ((p (make-derivability-problem (cons conjecture
						    (non-conjecture-formulas db)))))
	    (minimize p
		      :skip-initial-proof skip-initial-proof
		      :keep keep))
	  (minimize db
		    :skip-initial-proof skip-initial-proof
		    :keep keep)))))
