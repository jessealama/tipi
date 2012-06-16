
(in-package :tipi)

(defclass solver ()
  ((name
    :type string
    :initarg :name
    :accessor name
    :initform (error "To specify a solver, a name is mandatory."))))

(defgeneric solve (solver tptp-problem))

(defparameter *eprover* (make-instance 'solver
				       :name "The E theorem prover"))

(defmethod solve ((eprover (eql *eprover*)) (problem tptp-problem))
  (let ((eprover-text
	 (with-output-to-string (eprover-out)
	   (let ((eprover-process (run-program "eprover"
					       (list "-tAuto"
						     "-xAuto"
						     "-l4"
						     "-R"
						     "--tptp3-in"
						     (namestring (path problem)))
					       :search t
					       :input nil
					       :output eprover-out
					       :error :stream
					       :wait t)))
	     (let ((eprover-exit-code (process-exit-code eprover-process)))
	       (unless (zerop eprover-exit-code)
		 (error "eprover did not exit cleanly (its exit code was ~a).  The error output:~%~%~a" eprover-exit-code (stream-lines (process-error eprover-process)))))))))
    (let ((epclextract-text
	   (with-input-from-string (eprover-out eprover-text)
	     (with-output-to-string (epclextract-out)
	       (let ((epclextract-process (run-program "epclextract"
						       (list "--tstp-out")
						       :input eprover-out
						       :output epclextract-out
						       :error :stream
						       :wait t)))
		 (let ((epclextract-exit-code (process-exit-code epclextract-process)))
		   (unless (zerop epclextract-exit-code)
		     (error "epclextract did not exit cleanly (its exit code was ~a).  The error output:~%~%~a" epclextract-exit-code (stream-lines (process-error epclextract-process))))))))))
      (read-tptp epclextract-text))))
