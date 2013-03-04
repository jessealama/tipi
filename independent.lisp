
(in-package :tipi)

(defgeneric independent? (problem &key timeout quick)
  (:documentation "Determine whether the premises of PROBLEM are
  independent (none is implied by the others).

At most TIMEOUT seconds will be spend on any particular proof/model
search problem.  If QUICK is non-NIL, computation will stop as soon as
a single premise of PROBLEM is found to be derivable from the others.
Returns three values.  The first value is a boolean: T means that the
set of dependent premises is empty AND that the set of premises on
which we could not make a decision is also empty.  The second value is
a list of the premises that are derivable from the others.  The third
value is the set of premises for which we could not make a
decision."))

(defmethod independent? ((problem derivability-problem) &key timeout quick)
  "Throwing away the conjecture of PROBLEM, is the set of premises independent?"
  (independent? (make-instance 'tptp-db
			       :formulas (formulas problem))
		:timeout timeout
		:quick quick))

(defmethod independent? :around ((db tptp-db) &key timeout quick)
  (declare (ignore timeout quick))
  (multiple-value-bind (result dependent-premises unknown-premises)
      (call-next-method)
    (values result
	    (sort-formula-list dependent-premises)
	    (sort-formula-list unknown-premises))))

(defmethod independent? ((db tptp-db) &key timeout quick)
  (flet ((dependent? (premise)
	   (let* ((problem (make-instance 'derivability-problem
					  :conjecture premise
					  :formulas (formulas (remove-formula db premise))))
		  (szs-status (solve (list *eprover* *paradox*) problem :timeout timeout)))
	     (format t "SZS status of the removal of ~a: ~a~%" (name premise) szs-status)
	     (if (is-szs-success? szs-status)
		 (cond ((szs-implies? szs-status (lookup-szs-status "Theorem"))
			1)
		       ((szs-implies? szs-status (lookup-szs-status "CounterSatisfiable"))
			0)
		       (t
			-1))
		 -1))))
    (loop
       with dependent-premises = nil
       with independent-premises = nil
       with unknown-premises = nil
       for premise in (formulas db)
       for dependent = (dependent? premise)
       do
	 (cond ((= dependent -1)
		(push premise unknown-premises))
	       ((= dependent 0)
		(push premise independent-premises))
	       ((= dependent 1)
		(if quick
		    (return (values nil (list premise) unknown-premises))
		    (push premise dependent-premises))))
       finally
	 (return (values (and (null dependent-premises)
			      (null unknown-premises))
			 independent-premises
			 dependent-premises
			 unknown-premises)))))

(defmethod independent? ((problem pathname) &key timeout quick)
  (let ((db (parse-tptp problem)))
    (let ((conjecture (conjecture-formula db)))
      (independent? (if conjecture (remove-conjecture db) db)
		    :timeout timeout
		    :quick quick))))

(defgeneric completely-independent-p (problem &key timeout)
  (:documentation "Are the premises of PROBLEM completely independent?"))

(defmethod completely-independent-p :around (problem &key timeout)
  (cond ((null timeout)
	 (completely-independent-p problem
				   :timeout +default-timeout+))
	((integerp timeout)
	 (if (> timeout 0)
	     (call-next-method)
	     (error "Inappropriate non-positive value '~d' for the timeout argument." timeout)))
	(t
	 (error "Inappropriate argument '~a' for the timeout parameter." timeout))))

(defmethod completely-independent-p ((problem pathname) &key timeout)
  (completely-independent-p (parse-tptp problem)
			    :timeout timeout))

(defmethod completely-independent-p :before ((problem tptp-db) &key timeout)
  (declare (ignore timeout))
  (when (has-include-instruction-p problem)
    (error "The given problem has an include statement."))
  (when (has-conjecture-p problem)
    (error "The given problem has a conjecture formula, but we require that no conjecture formulas are present.")))

(defmethod completely-independent-p ((problem tptp-db) &key timeout)
  (let ((formulas (formulas problem)))
    (let ((definitions (remove-if-not #'definition-p formulas))
	  (non-definitions (remove-if #'definition-p formulas)))
      (flet ((test-combination (in)
	       (loop
		  :initially (format t "[")
		  :with unnegated = (append in definitions)
		  :for formula :in formulas
		  :do (format t "~:[-~;+~]" (member formula unnegated))
		  :finally (format t "]: "))
	       (let ((out (remove-if #'(lambda (x) (member x in)) non-definitions)))
		 (let ((new-formulas (append definitions in (negate out))))
		   (let ((subproblem (make-instance 'tptp-db :formulas new-formulas)))
		     (let ((sat (satisfiable-p subproblem :timeout timeout)))
		       (format t "~:[no~;yes~]~%" sat)
		       sat))))))
	(map-all-combinations #'test-combination non-definitions)))))
