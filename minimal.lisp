
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

(defgeneric minimize (problem &key skip-initial-proof))

(defmethod minimize :around ((problem derivability-problem) &key skip-initial-proof)
  (if skip-initial-proof
      (call-next-method)
      (let ((initial-result (solve *eprover* problem)))
	(let ((status (szs-status initial-result)))
	  (if (szs-implies? status (lookup-szs-status "Theorem"))
	      (let ((result-as-db (interpret initial-result)))
		(let ((used-premises (used-premises result-as-db problem)))
		  (format t "The initial derivability problem of ~d premises has a solution using ~a premises" (length (formulas problem)) (length used-premises))
		  (let ((trimmed-problem (make-instance 'derivability-problem
							:conjecture (conjecture problem)
							:formulas (mapcar #'(lambda (name) (formula-with-name problem name)) used-premises))))
		    (minimize trimmed-problem :skip-initial-proof t))))
	      (error "The initial problem could not be solved (SZS status ~a)." status))))))

(defmethod minimize ((problem derivability-problem) &key skip-initial-proof)
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
		 (let ((smaller-problem (make-instance 'derivability-problem
						       :conjecture conjecture
						       :formulas subset)))
		   (let ((szs-status (solve (list *eprover* *paradox*) smaller-problem)))
		     (if (is-szs-success? szs-status)
			 (if (szs-implies? szs-status
					   (lookup-szs-status "Theorem"))
			     (progn
			       (push subset solutions)
			       (format t "Solution: ~a" (mapcar #'name subset)))
			     (progn
			       (push subset non-solutions)
			       (format t "Non-solution: ~a" (mapcar #'name subset))))
			 (progn
			   (push subset unknown)
			   (format t "Unknown: ~a" (mapcar #'name subset))))
		     (terpri))))))
      (loop
	 for n from 0 upto (length premises)
	 do
	   (map-combinations #'test-subset premises :length n)
	 finally
	   (return (values solutions non-solutions unknown))))))
