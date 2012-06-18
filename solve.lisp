
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

(defparameter *paradox* (make-instance 'solver
				       :name "Paradox"))

(defmethod solve :before (prover (problem tptp-problem))
  (declare (ignore prover))
  (unless (slot-boundp problem 'path)
    (let ((temp (temporary-file)))
      (write-string-into-file (render problem) temp)
      (setf (path problem) temp))))

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
				   (error "epclextract did not exit cleanly (its exit code was ~a).  The error output:~%~%~a" epclextract-exit-code (stream-lines (process-error epclextract-process)))))))))))

(defmethod solve ((paradox (eql *paradox*)) (problem tptp-problem))
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
