
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

(defgeneric needed-premise? (premise problem)
  (:documentation "Can PROBLEM be solved without PREMISE?"))

(defmethod needed-premise? ((premise tptp-formula)
			    (problem derivability-problem))
  (let* ((problem (remove-formula problem premise))
	 (szs-status (solve (list *eprover* *paradox*) problem))
	 (implies-theorem (szs-implies? szs-status
					(lookup-szs-status "Theorem"))))
    (values (not implies-theorem) szs-status)))

(defmethod needed-premise? ((formula-name string)
			   (premises tptp-db))
  (needed-premise? (formula-with-name premises formula-name) premises))

(defmethod needed-premise? ((formula symbol) premises)
  (needed-premise? (symbol-name formula) premises))

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

(defgeneric needed-premises (problem))

(defmethod needed-premises ((problem derivability-problem))
  (loop
     with needed-premises = nil
     for premise in (formulas problem)
     do
       (multiple-value-bind (needed? szs-status)
	   (needed-premise? premise problem)
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
