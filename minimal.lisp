
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
      (let ((initial-result (solve *eprover* problem)))
	(let ((status (szs-status initial-result)))
	  (if (szs-implies? status (lookup-szs-status "Theorem"))
	      (let ((result-as-db (interpret initial-result)))
		(let ((used-premises (used-premises result-as-db problem)))
		  (let ((trimmed-problem (make-instance 'derivability-problem
							:conjecture (conjecture problem)
							:formulas (mapcar #'(lambda (name) (formula-with-name problem name)) used-premises))))
		    (let ((needed (needed-premises trimmed-problem)))
		      (minimize trimmed-problem
				:skip-initial-proof t
				:keep needed)))))
	      (error "The initial problem could not be solved (SZS status ~a)." status))))))

(defmethod minimize ((problem derivability-problem) &key skip-initial-proof keep)
  (declare (ignore skip-initial-proof))
  (let ((premises (premises problem))
	(conjecture (conjecture problem))
	(solutions nil)
	(unknown nil)
	(non-solutions nil))
    (labels ((known-solution (solution)
	       (some #'(lambda (other-subset) (subsetp other-subset solution))
			   solutions))
	     (test-subset (subset)
	       (unless (known-solution subset)
		 (when (subsetp keep subset)
		   (let ((smaller-problem (make-instance 'derivability-problem
							 :conjecture conjecture
							 :formulas subset)))
		     (let ((szs-status (solve (list *eprover* *paradox*) smaller-problem)))
		       (if (is-szs-success? szs-status)
			   (if (szs-implies? szs-status
					     (lookup-szs-status "Theorem"))
			       (push subset solutions)
			       (push subset non-solutions))
			   (push subset unknown))))))))
      (loop
	 for n from 0 upto (length premises)
	 do
	   (map-combinations #'test-subset premises :length n)
	 finally
	   (return (values solutions non-solutions unknown))))))

(defmethod minimize ((problem pathname) &key skip-initial-proof keep)
  (let ((db (read-tptp problem)))
    (let ((conjecture (conjecture-formula db)))
      (if conjecture
	  (let ((problem (make-instance 'derivability-problem
					:conjecture conjecture
					:formulas (non-conjecture-formulas db))))
	    (minimize problem
		      :skip-initial-proof skip-initial-proof
		      :keep keep))
	  (minimize db
		    :skip-initial-proof skip-initial-proof
		    :keep keep)))))
