
(in-package :tipi)

(defun satisfiable? (problem &optional (solver *paradox*))
  (let ((conjecture (has-conjecture-formula? problem)))
    (if conjecture
	(satisfiable? (promote-conjecture-to-axiom problem) solver)
	(let ((result (solve solver problem)))
	  (let ((szs (szs-status result)))
	    (values (szs-implies? szs (lookup-szs-status "Satisfiable"))
		    szs))))))

(defun consistent-premises? (problem &optional (solver *paradox*))
  (satisfiable? (remove-conjecture problem) solver))

(defun find-model (problem &optional (model-finder *paradox*))
  (let ((conjecture (has-conjecture-formula? problem)))
    (if conjecture
	(find-model (promote-conjecture-to-axiom problem) model-finder)
	(let ((result (solve model-finder problem)))
	  (let ((szs (szs-status result)))
	    (if (is-szs-success? szs)
		(interpret result)
		(error "Something went wrong applying ~a to ~a" (name model-finder) problem)))))))
