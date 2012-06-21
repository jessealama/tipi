
(in-package :tipi)

(define-constant +default-timeout+
    5
  :test #'=
  :documentation "The number of seconds given to a solver, by default.")

(defclass solver ()
  ((name
    :type string
    :initarg :name
    :accessor name
    :initform (error "To specify a solver, a name is mandatory."))
   (solve-function
    :type function
    :initarg :solve-function
    :accessor solve-function
    :initform (error "To specify a solver, one must say how it solves problems."))))

(defmethod print-object ((solver solver) stream)
  (print-unreadable-object (solver stream :type t :identity nil)
    (format stream "~a" (name solver))))

(defgeneric solve (solver tptp-db &key timeout))

(defparameter *eprover*
  (make-instance
   'solver
   :name "The E theorem prover"
   :solve-function
   (lambda (problem timeout)
     (block eprover
       (let ((eprover-text
	      (with-output-to-string (eprover-out)
		(let ((eprover-process (run-program "eprover"
						    (list "-tAuto"
							  "-xAuto"
							  "-l4"
							  "-R"
							  "--tptp3-in"
							  (format nil "--cpu-limit=~d" timeout)
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
							"Error"))))))))))
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
					(error "epclextract did not exit cleanly (its exit code was ~a).  The error output:~%~%~a" epclextract-exit-code (stream-lines (process-error epclextract-process))))))))))))))

(defparameter *paradox*
  (make-instance
   'solver
   :name "Paradox"
   :solve-function
   (lambda (problem timeout)
     (block paradox
       (let ((paradox-text
	      (with-output-to-string (paradox-out)
		(let ((paradox-process (run-program "paradox"
						    (list "--model"
							  "--tstp"
							  "--no-progress"
							  "--time" (format nil "~d" timeout)
							  (namestring (path problem)))
						    :search t
						    :input nil
						    :output paradox-out
						    :error :stream
						    :wait t)))
		  (let ((paradox-exit-code (process-exit-code paradox-process)))
		    (unless (zerop paradox-exit-code)
		      (return-from paradox
			(make-instance 'paradox-result
				       :text ""
				       :szs-status (lookup-szs-status "Error")))))))))
	 (make-instance 'paradox-result
			:text paradox-text))))))

(defmethod solve :around (prover (problem tptp-db) &key timeout)
  (declare (ignore prover timeout))
  (if (slot-boundp problem 'path)
      (call-next-method)
      (let ((temp (temporary-file)))
	(write-string-into-file (render problem) temp)
	(setf (path problem) temp)
	(prog1
	    (call-next-method)
	  (delete-file temp)))))

(defmethod solve ((solver-list null) problem &key timeout)
  (declare (ignore problem timeout))
  (values nil (lookup-szs-status "Unknown")))

(defmethod solve ((solver-list list) problem &key timeout)
  (unless timeout
    (setf timeout +default-timeout+))
  (let ((solutions (make-hash-table :test #'equal)))
    (dolist (solver solver-list)
      (setf (gethash (name solver) solutions) (lookup-szs-status "NotTriedYet")))
    (loop
       for solver in solver-list
       for result = (solve solver problem :timeout timeout)
       for szs-status = (szs-status result)
       do
	 (setf (gethash (name solver) solutions) szs-status)
	 (when (is-szs-success? szs-status)
	   (return (aggregate-szs-statuses (hash-table-values solutions))))
       finally
	 (return (lookup-szs-status "Unknown")))))

(defmethod solve ((solver solver) (problem derivability-problem) &key timeout)
  (unless timeout
    (setf timeout +default-timeout+))
  (unless (integerp timeout)
    (error "Invalid value ~a for the timeout parameter." timeout))
  (when (< timeout 1)
    (error "Invalid value ~a for the timeout parameter." timeout))
  (let ((db (make-instance 'tptp-db
			   :formulas (cons (conjecture problem)
					   (formulas problem)))))
    (solve solver db :timeout timeout)))

(defmethod solve ((solver solver) (problem tptp-db) &key timeout)
  (funcall (solve-function solver) problem timeout))

(defmethod solve (solver (problem pathname) &key timeout)
  (solve solver (read-tptp problem) :timeout timeout))
