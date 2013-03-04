
(in-package :tipi)

(defgeneric satisfiable-p (tptp &key timeout)
  (:documentation "Is TPTP satisfiable?"))

(defmethod satisfiable-p ((path pathname) &key timeout)
  (satisfiable-p (parse-tptp path) :timeout timeout))

(defmethod satisfiable-p :around ((db tptp-db) &key timeout)
  (if (has-conjecture-p db)
      (satisfiable-p (promote-conjecture-to-axiom db) :timeout timeout)
      (call-next-method)))

(defmethod satisfiable-p ((db tptp-db) &key timeout)
  (setf timeout (or timeout +default-timeout+))
  (let ((szs (solve-problem db :timeout timeout)))
    (values (szs-implies? szs (lookup-szs-status "Satisfiable"))
	    szs)))

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
