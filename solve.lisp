
(in-package :tipi)

(defclass solver ()
  ((name
    :type string
    :initarg :name
    :accessor name
    :initform (error "To specify a solver, a name is mandatory."))))

(defmethod print-object ((solver solver) stream)
  (print-unreadable-object (solver stream :type t :identity nil)
    (format stream "~a" (name solver))))

(defgeneric solve (solver tptp-db))

(defparameter *eprover* (make-instance 'solver
				       :name "The E theorem prover"))

(defparameter *paradox* (make-instance 'solver
				       :name "Paradox"))

(defmethod solve :before (prover (problem tptp-db))
  (declare (ignore prover))
  (unless (slot-boundp problem 'path)
    (let ((temp (temporary-file)))
      (write-string-into-file (render problem) temp)
      (setf (path problem) temp))))

(defmethod solve ((solver-list null) problem)
  (declare (ignore problem))
  (values nil (lookup-szs-status "Unknown")))

(defmethod solve ((solver-list list) problem)
  (let ((solutions (make-hash-table :test #'equal)))
    (dolist (solver solver-list)
      (setf (gethash (name solver) solutions) (lookup-szs-status "NotTriedYet")))
    (loop
       for solver in solver-list
       for result = (solve solver problem)
       for szs-status = (szs-status result)
       do
	 (setf (gethash (name solver) solutions) szs-status)
	 (when (is-szs-success? szs-status)
	   (return (aggregate-szs-statuses (hash-table-values solutions))))
       finally
	 (return (lookup-szs-status "Unknown")))))

(defmethod solve ((eprover (eql *eprover*)) (problem tptp-db))
  (block eprover
    (let ((eprover-text
	   (with-output-to-string (eprover-out)
	     (let ((eprover-process (run-program "eprover"
						 (list "-tAuto"
						       "-xAuto"
						       "-l4"
						       "-R"
						       "--tptp3-in"
						       "--cpu-limit=10"
						       (namestring (path problem)))
						 :search t
						 :input nil
						 :output eprover-out
						 :error :stream
						 :wait t)))
	       (let ((eprover-exit-code (process-exit-code eprover-process)))
		 (unless (zerop eprover-exit-code)
		   (return-from eprover
		     (make-instance 'eprover-result
				    :text ""
				    :szs-status (lookup-szs-status
						 (if (= eprover-exit-code 6)
						     "Timeout"
						     "Unknown"))))))))))
    (make-instance 'eprover-result
		   :text (with-output-to-string (epclextract-out)
			   (with-input-from-string (eprover-out eprover-text)
			     (let ((epclextract-process (run-program "epclextract"
								     (list "--tstp-out"
									   "--forward-comments")
								     :input eprover-out
								     :output epclextract-out
								     :error :stream
								     :wait t)))
			       (let ((epclextract-exit-code (process-exit-code epclextract-process)))
				 (unless (zerop epclextract-exit-code)
				   (error "epclextract did not exit cleanly (its exit code was ~a).  The error output:~%~%~a" epclextract-exit-code (stream-lines (process-error epclextract-process))))))))))))

(defmethod solve ((paradox (eql *paradox*)) (problem tptp-db))
  (let ((paradox-text
	 (with-output-to-string (paradox-out)
	   (let ((paradox-process (run-program "paradox"
					       (list "--model"
						     "--tstp"
						     (namestring (path problem)))
					       :search t
					       :input nil
					       :output paradox-out
					       :error :stream
					       :wait t)))
	     (let ((paradox-exit-code (process-exit-code paradox-process)))
	       (unless (zerop paradox-exit-code)
		 (error "paradox did not exit cleanly (its exit code was ~a).  The error output:~%~%~a" paradox-exit-code (stream-lines (process-error paradox-process)))))))))
    (make-instance 'paradox-result
		   :text paradox-text)))
