
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
			    (problem derivability-problem)
			    &key timeout)
  (let* ((problem (remove-formula problem premise))
	 (szs-status (solve (list *eprover* *paradox*) problem :timeout timeout))
	 (implies-theorem (szs-implies? szs-status
					(lookup-szs-status "Theorem"))))
    (values (not implies-theorem) szs-status)))

(defmethod needed-premise? ((formula-name string)
			   (premises tptp-db)
			    &key timeout)
  (needed-premise? (formula-with-name premises formula-name) premises
		   :timeout timeout))

(defmethod needed-premise? ((formula symbol) premises &key timeout)
  (needed-premise? (symbol-name formula) premises :timeout timeout))

(defgeneric extraneous-premises (solution theorem background-theory))

(defmethod extraneous-premises ((solution tptp-db)
				(conjecture tptp-formula)
				(background-premises tptp-db))
  (extraneous-premises (used-premises solution background-premises)
		       conjecture
		       background-premises))

(defmethod extraneous-premises ((solution list)
				(conjecture tptp-formula)
				(background-premises derivability-problem))
  (remove-if #'(lambda (premise)
		 (needed-premise? premise background-premises))
	     (remove-if #'(lambda (sol) (equal-as-strings? sol
							   (name conjecture)))
			solution)))

(defgeneric needed-premises (problem &key timeout))

(defmethod needed-premises ((problem derivability-problem) &key timeout)
  (loop
     with needed-premises = nil
     with unknown-premises = nil
     for premise in (formulas problem)
     do
       (multiple-value-bind (needed? szs-status)
	   (needed-premise? premise problem :timeout timeout)
	 (if (is-szs-success? szs-status)
	     (when needed?
	       (push premise needed-premises))
	     (push premise unknown-premises)))
     finally
       (return (list needed-premises unknown-premises))))

(defmethod needed-premises ((db tptp-db) &key timeout)
  (let ((conjecture (conjecture-formula db)))
    (if conjecture
	(let ((problem (make-instance 'derivability-problem
				      :conjecture conjecture
				      :formulas (non-conjecture-formulas db))))
	  (needed-premises problem :timeout timeout))
	(error "There is no conjecture formula in the given problem."))))

(defmethod needed-premises ((path pathname) &key timeout)
  (let ((db (read-tptp path)))
    (needed-premises db :timeout timeout)))
