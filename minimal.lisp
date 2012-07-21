
(in-package :tipi)

(defgeneric minimize (problem &key timeout))

(defmethod minimize ((problem derivability-problem) &key (timeout +default-timeout+))
  (let ((conjecture (conjecture problem))
	(solutions nil)
	(unknown nil)
	(non-solutions nil))
    (destructuring-bind (needed unneeded needed-unknown)
	(needed-premises problem :timeout timeout)
      (if needed
	  (if (rest needed)
	      (format t "~d needed premises:~%~%~{~a~%~}~%" (length needed) (mapcar #'name needed))
	      (format t "1 needed premise:~%~%~{~%~}~%" (mapcar #'name needed)))
	  (format t "Needed premises:~%~%(none we shown to be needed)~%"))
      (labels ((formula-subset (formula-set-1 formula-set-2)
		 (subsetp formula-set-1 formula-set-2 :test #'string=))
	       (already-known (subset)
		 (or (some #'(lambda (solution)
			       (formula-subset solution subset))
			   solutions)
		     (some #'(lambda (non-solution)
			       (formula-subset subset non-solution))
			   non-solutions)))
	     (test-subset (subset)
	       (if (already-known subset)
		   (progn
		     (format t "Skipping a settled subset.~%")
		     )
		   (let* ((p (make-instance 'derivability-problem
					    :conjecture conjecture
					    :formulas (append needed
							      (mapcar #'(lambda (name)
									  (formula-with-name problem name))
								      subset))))
			  (szs-status (solve-problem p :timeout timeout)))
		     (if (is-szs-success? szs-status)
			 (if (szs-implies? szs-status (lookup-szs-status "Theorem"))
			     (progn
			       (push subset solutions)
			       (format t "Solution:~%~{~a~%~}~%" subset)
			       )
			     (progn
			       (push subset non-solutions)
			       (format t "Non-solution:~%~{~a~%~}~%" subset)
			       ))
			 (progn
			   (push subset unknown)
			   (format t "Unknown:~%~{~a~%~}~%" subset)
			   ))))))
	(map-all-combinations #'test-subset
			      (mapcar #'name (append unneeded needed-unknown)))))
    (list solutions non-solutions unknown)))

(defmethod minimize ((problem pathname) &key (timeout +default-timeout+))
  (let ((db (read-tptp problem)))
    (let ((conjecture (conjecture-formula db)))
      (if conjecture
	  (let ((p (make-derivability-problem (cons conjecture
						    (non-conjecture-formulas db)))))
	    (minimize p
		      :timeout timeout))
	  (minimize db
		    :timeout timeout)))))
