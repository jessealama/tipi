
(in-package :tipi)

(defgeneric satisfiable-p (tptp &key timeout)
  (:documentation "Is TPTP satisfiable?"))

(defmethod satisfiable-p ((path pathname) &key timeout)
  (satisfiable-p (parse-tptp path) :timeout timeout))

(defmethod satisfiable-p ((db tptp-db) &key timeout)
  (unless timeout
    (setf timeout +default-timeout+))
  (let ((conjecture (conjecture-formula db)))
    (if conjecture
	(satisfiable-p (promote-conjecture-to-axiom db) :timeout timeout)
	(let ((szs (solve-problem db :timeout timeout)))
	  (szs-implies? szs (lookup-szs-status "Satisfiable"))))))

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
