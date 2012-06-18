
(in-package :tipi)

(defgeneric consequence-of? (formula premises))

(defmethod consequence-of? ((formula formula) (premises null))
  (let ((tptp-formula (make-instance 'tptp-formula
				     :name "goal"
				     :syntax "formula"
				     :status "conjecture"
				     :formula formula)))
    (consequence-of? tptp-formula premises)))

(defmethod consequence-of? ((formula tptp-formula) (premises null))
  (let ((problem (make-instance 'tptp-problem
				:formulas (list formula))))
    (solve *eprover* problem)))

(defmethod consequence-of? ((formula tptp-formula) (problem tptp-problem))
  (let ((conjecture-as-axiom (promote-conjecture-to-axiom problem)))
    (let ((proper-formulas (proper-formulas conjecture-as-axiom))
	  (formula-proper (formula formula)))
      (consequence-of? formula-proper proper-formulas))))

(defmethod consequence-of? ((formula formula) (premises list))
  (let ((formulas (loop
		     for i from 1 upto (length premises)
		     for premise in premises
		     collect (make-instance 'tptp-formula
					    :name (format nil "ax~d" i)
					    :syntax "formula"
					    :status "axiom"
					    :formula premise) into tptp-formulas
		     finally (return tptp-formulas))))
    (let ((conjecture (make-instance 'tptp-formula
				     :name "goal"
				     :syntax "formula"
				     :status "conjecture"
				     :formula formula)))
      (let ((problem (make-instance 'tptp-problem
				    :formulas (cons conjecture formulas))))
	(let ((result (solve *eprover* problem)))
	  (let ((status (szs-status result)))
	    (values (szs-implies? status (lookup-szs-status "Theorem"))
		    status)))))))
