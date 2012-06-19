
(in-package :tipi)

(defgeneric used-premises (solution premise-pool))

(defmethod used-premises ((solution tptp-db) (background-theory tptp-db))
  (loop
     with background-premises = (formula-names background-theory)
     with used-table = (make-hash-table :test #'equal)
     for solution-formula in (formulas solution)
     when (slot-boundp solution-formula 'source) do
       (loop
	  with used-names = (atoms-in-list (source solution-formula)
					   background-premises
					   :test #'equal-as-strings?)
	  for used in used-names do (setf (gethash used used-table) 0))
     finally
       (return (hash-table-keys used-table))))

(defgeneric needed-premise? (formula theorem premises)
  (:documentation "Is it true that PREMISES, minus FORMULA, entails THEOREM?"))

(defmethod needed-premise? ((formula tptp-formula)
			    (theorem tptp-formula)
			    (premises tptp-db))
  (let ((premises-no-conjecture (remove-conjecture premises))
	(theorem-as-conjecture (change-status theorem "conjecture")))
    (let ((trimmed-db (remove-formula premises-no-conjecture formula)))
      (let ((problem (make-instance 'tptp-db
				    :formulas (cons theorem-as-conjecture
						    (formulas trimmed-db)))))
	(let ((szs-status (solve (list *eprover* *paradox*) problem)))
	  (let ((implies-theorem (szs-implies? szs-status
					       (lookup-szs-status "Theorem"))))
	    (if implies-theorem
		(values (not implies-theorem) szs-status)
		(values t szs-status))))))))

(defmethod needed-premise? ((formula-name string)
			   theorem
			   (premises tptp-db))
  (needed-premise? (formula-with-name premises formula-name) theorem premises))

(defmethod needed-premise? ((formula symbol) theorem premises)
  (needed-premise? (symbol-name formula) theorem premises))

(defmethod needed-premise? (formula (theorem-name string) (premises tptp-db))
  (let ((formula-in-db (formula-with-name premises theorem-name)))
    (if formula-in-db
	(needed-premise? formula formula-in-db premises)
	(error "There is no formula in ~a by the name '~a'." premises theorem-name))))

(defgeneric extraneous-premises (solution theorem background-theory))

(defmethod extraneous-premises ((solution tptp-db)
				(conjecture tptp-formula)
				(background-premises tptp-db))
  (extraneous-premises (used-premises solution background-premises)
		       conjecture
		       background-premises))

(defmethod extraneous-premises ((solution list)
				(conjecture tptp-formula)
				(background-premises tptp-db))
  (remove-if #'(lambda (premise)
		 (needed-premise? premise conjecture background-premises))
	     (remove-if #'(lambda (sol) (equal-as-strings? sol
							   (name conjecture)))
			solution)))

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

(defgeneric needed-premises (conjecture premises))

(defmethod needed-premises ((conjecture tptp-formula)
			    (premises tptp-db))
  (loop
     with needed-premises = nil
     for premise in (formulas (remove-conjecture premises))
     do
       (multiple-value-bind (needed? szs-status)
	   (needed-premise? premise
			    conjecture
			    (remove-formula premises premise))
	 (if (is-szs-success? szs-status)
	     (progn
	       (when needed?
		 (push premise needed-premises))
	       (if needed?
		   (format t "Premise ~a is needed (SZS status ~a)." (name premise) szs-status)
		   (format t "Premise ~a is not needed (SZS status ~a)." (name premise) szs-status)))
	     (format t "It is not known whether premise ~a is needed (SZS status ~a)" (name premise) szs-status)))
       (terpri)
     finally
       (return needed-premises)))
