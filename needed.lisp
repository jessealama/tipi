
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

(defgeneric needed-premise? (premise problem &key timeout)
  (:documentation "Can PROBLEM be solved without PREMISE?"))

(defmethod needed-premise? ((premise tptp-formula)
			    (problem tptp-db)
			    &key timeout)
  (let* ((new-problem (remove-formula problem premise))
	 (szs-status (solve-problem new-problem :timeout timeout))
	 (implies-theorem (szs-implies? szs-status
					(lookup-szs-status "Theorem"))))
    (values (and (is-szs-success? szs-status)
		 (not implies-theorem))
	    szs-status)))

(defmethod needed-premise? ((formula-name string)
			   (premises tptp-db)
			    &key timeout)
  (let ((formula (formula-with-name premises formula-name)))
    (if formula
	(needed-premise? formula premises :timeout timeout)
	(error "No such formula by the name '~a'." formula-name))))

(defmethod needed-premise? ((formula symbol) premises &key timeout)
  (needed-premise? (symbol-name formula) premises :timeout timeout))

(defmethod needed-premise? ((formula string) (premises pathname) &key timeout)
  (needed-premise? formula (parse-tptp premises) :timeout timeout))

(defgeneric needed-premises (problem &key timeout))

(defmethod needed-premises ((problem derivability-problem) &key (timeout +default-timeout+))
  (loop
     with needed-premises = nil
     with unneeded-premises = nil
     with unknown-premises = nil
     for premise in (formulas problem)
     do
       (multiple-value-bind (needed? szs-status)
	   (needed-premise? premise problem :timeout timeout)
	 (if (is-szs-success? szs-status)
	     (if needed?
		 (push premise needed-premises)
		 (push premise unneeded-premises))
	     (push premise unknown-premises)))
     finally
       (return (list needed-premises unneeded-premises unknown-premises))))

(defmethod needed-premises :before ((db tptp-db) &key timeout)
  (declare (ignore timeout))
  (when (has-include-instruction-p db)
    (error "The given problem has an include instruction; please supply a fully expanded problem.")))

(defmethod needed-premises ((db tptp-db) &key (timeout +default-timeout+))
  (needed-premises (make-instance 'derivability-problem
				  :conjecture (conjecture-formula db)
				  :formulas (non-conjecture-formulas db))
		   :timeout timeout))

(defmethod needed-premises ((path pathname) &key (timeout +default-timeout+))
  (let ((db (parse-tptp path)))
    (needed-premises db :timeout timeout)))
