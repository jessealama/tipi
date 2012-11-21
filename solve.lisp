
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
       (let* ((path (native-namestring (path problem)))
	      (eprover-out (make-string-output-stream))
	      (eprover-process
	       (run-program "eprover"
			    (list "-tAuto"
				  "-xAuto"
				  "-l4"
				  "-R"
				  "--tptp3-in"
				  (format nil "--cpu-limit=~d" timeout)
				  path)
			    :search t
			    :input nil
			    :output eprover-out
			    :error :stream
			    :wait t))
	      (eprover-exit-code (process-exit-code eprover-process)))
	 (unless (zerop eprover-exit-code)
	   (return-from eprover
	     (make-instance 'eprover-result
			    :text ""
			    :szs-status (lookup-szs-status
					 (if (= eprover-exit-code 6)
					     "Timeout"
					     "Error")))))
	 (let ((eprover-text (get-output-stream-string eprover-out))
	       (epclextract-out (make-string-output-stream)))
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
		   (error "epclextract did not exit cleanly (its exit code was ~a).  The error output:~%~%~a" epclextract-exit-code (stream-lines (process-error epclextract-process)))))))
	   (make-instance 'eprover-result
			  :text (get-output-stream-string epclextract-out))))))))

(defparameter *paradox*
  (make-instance
   'solver
   :name "Paradox"
   :solve-function
   (lambda (problem timeout)
     (block paradox
       (let* ((path (native-namestring (path problem)))
	      (paradox-out (make-string-output-stream))
	      (paradox-process (run-program "paradox"
					    (list "--model"
						  "--tstp"
						  "--no-progress"
						  "--time" (format nil "~d" timeout)
						  path)
					    :search t
					    :input nil
					    :output paradox-out
					    :error :stream
					    :wait t))
	      (paradox-exit-code (process-exit-code paradox-process)))
	 (if (zerop paradox-exit-code)
	     (make-instance 'paradox-result
			    :text (get-output-stream-string paradox-out))
	     (make-instance 'paradox-result
			    :text ""
			    :szs-status (lookup-szs-status "Error"))))))))

(defmethod solve :before (prover problem &key (timeout +default-timeout+))
  (declare (ignore prover problem))
  (unless (integerp timeout)
    (error "Invalid value ~a for the timeout parameter." timeout))
  (when (< timeout 1)
    (error "Invalid value ~a for the timeout parameter." timeout)))

(defmethod solve :around (prover (problem tptp-db) &key (timeout +default-timeout+))
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
  (declare (ignore timeout problem))
  (values nil (lookup-szs-status "Unknown")))

(defmethod solve ((solver-list list) problem &key (timeout +default-timeout+))
  (loop
     with solutions = (make-hash-table :test #'equal)
     initially
       (dolist (solver solver-list)
	 (setf (gethash (name solver) solutions)
	       (lookup-szs-status "NotTriedYet")))
     for solver in solver-list
     for result = (solve solver problem :timeout timeout)
     for szs-status = (szs-status result)
     do
       (setf (gethash (name solver) solutions) szs-status)
       (when (is-szs-success? szs-status)
	 (return (aggregate-szs-statuses (hash-table-values solutions))))
     finally
       (return (lookup-szs-status "Unknown"))))

(defgeneric solve-problem (problem &key timeout))

(defmethod solve-problem ((problem derivability-problem)
			  &key (timeout +default-timeout+))
  (let ((temp (temporary-file)))
    (write-string-into-file (render problem) temp)
    (prog1
	(solve-problem temp :timeout timeout)
      ;; (delete-file temp)
      )))

(defmethod solve-problem ((problem pathname) &key (timeout +default-timeout+))
  (let ((path (native-namestring problem))
	(eprover-out (make-string-output-stream))
	(paradox-out (make-string-output-stream))
	(granularity 5))
    (let ((eprover-process
	   (run-program "eprover"
			(list "-tAuto"
			      "-xAuto"
			      "-l4"
			      "-R"
			      "--tptp3-in"
			      (format nil "--cpu-limit=~d" timeout)
			      path)
			:search t
			:input nil
			:output eprover-out
			:error :stream
			:wait nil))
	  (paradox-process (run-program "paradox"
					(list "--model"
					      "--tstp"
					      "--no-progress"
					      "--time" (format nil "~d" timeout)
					      path)
					:search t
					:input nil
					:output paradox-out
					:error :stream
					:wait nil)))
      (loop
	 with results-table = (make-hash-table :test #'equal)
	 with process-table = (make-hash-table :test #'equal)
	 initially
	   (dolist (solver (list "paradox" "eprover"))
	     (setf (gethash solver results-table)
		   (lookup-szs-status "NotTriedYet")))
	   (setf (gethash "eprover" process-table) eprover-process)
	   (setf (gethash "paradox" process-table) paradox-process)
	 for i from 1 upto (* granularity timeout)
	 do
	   (sleep (float (/ 1 granularity)))
	   (let (eprover-status eprover-exit)
	     #+ccl
	     (multiple-value-bind (ccl-eprover-status ccl-eprover-exit)
	       (ccl:external-process-status eprover-process)
	       (setf eprover-status ccl-eprover-status
		     eprover-exit ccl-eprover-exit))
	     #+sbcl
	     (setf eprover-status (sb-ext:process-status eprover-process)
		   eprover-exit (sb-ext:process-exit-code eprover-process))
	     #-(or sbcl ccl)
	     (error "We handle only CCL and SBCL.")
	     (if (not (eql eprover-status :running))
	       (if (zerop eprover-exit)
		   (let ((eprover-text (get-output-stream-string eprover-out))
			 (epclextract-out (make-string-output-stream)))
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
			     (error "epclextract did not exit cleanly (its exit code was ~a).  The error output:~%~%~a" epclextract-exit-code (stream-lines (process-error epclextract-process)))))))
		     (let ((result (make-instance 'eprover-result
						  :text (get-output-stream-string epclextract-out))))
		       (let ((status (szs-status result)))
			 (setf (gethash "eprover" results-table)
			       (or status
				   (lookup-szs-status "Unknown"))))))
		   (setf (gethash "eprover" results-table)
			 (lookup-szs-status "Error")))))
	   (let (paradox-status paradox-exit)
	     #+ccl
	     (multiple-value-bind (ccl-paradox-status ccl-paradox-exit)
	       (ccl:external-process-status paradox-process)
	       (setf paradox-status ccl-paradox-status
		     paradox-exit ccl-paradox-exit))
	     #+sbcl
	     (setf paradox-status (sb-ext:process-status paradox-process)
		   paradox-exit (sb-ext:process-exit-code paradox-process))
	     #-(or sbcl ccl)
	     (error "We handle onl CCL and SBCL")
	     (if (not (eql paradox-status :running))
		 (if (zerop paradox-exit)
		     (let ((result (make-instance 'paradox-result
						  :text (get-output-stream-string paradox-out))))
		       (let ((status (szs-status result)))
			 (setf (gethash "paradox" results-table)
			       (or status
				   (lookup-szs-status "Unknown")))))
		     (setf (gethash "paradox" results-table)
			   (lookup-szs-status "Error")))))
	   (when (some #'(lambda (status)
			   (szs-implies? status (lookup-szs-status "Theorem")))
			  (hash-table-values results-table))
	     (loop
		for process in (hash-table-values process-table)
		for status = #+ccl (ccl:external-process-status process)
		             #+sbcl (sb-ext:process-status process)
		             #-(or sbcl ccl) (error "We handle only SBCL and CCL.")
		when (eql status :running) do
		  #+ccl (ccl:signal-external-process process 1)
		  #+sbcl (sb-ext:process-kill process 1)
		  #-(or sbcl ccl) (error "We handle only SBCL and CCL.")
		  )
	     (return (aggregate-szs-statuses (hash-table-values results-table))))
	   (when (every #'(lambda (status)
			    (not (eql status :running)))
			(mapcar
			 #+ccl
			 #'ccl:external-process-status
			 #+sbcl
			 #'sb-ext:process-status
			 #-(or sbcl ccl)
			 (error "We handle only SBCL and CCL.")
				(hash-table-values process-table)))
	     (return (aggregate-szs-statuses (hash-table-values results-table))))
	 finally
	   (loop
	      for process in (hash-table-values process-table)
	      for status =
		#+ccl
		(ccl:external-process-status process)
		#+sbcl
		(sb-ext:process-status process)
		#-(or sbcl ccl)
		(error "We handle only SBCL and CCL.")
	      when (eql status :running) do
		#+ccl
		(ccl:signal-external-process process 1)
		#+sbcl
		(sb-ext:process-kill process 1)
		#-(or sbcl ccl)
		(error "We handle only SBCL and CCL.")
		)
	   (return (lookup-szs-status "Timeout"))))))


(defmethod solve ((solver solver) (problem derivability-problem) &key (timeout +default-timeout+))
  (let ((db (make-instance 'tptp-db
			   :formulas (cons (conjecture problem)
					   (formulas problem)))))
    (solve solver db :timeout timeout)))

(defmethod solve ((solver solver) (problem tptp-db) &key (timeout +default-timeout+))
  (handler-case
      (with-timeout (timeout)
	(handler-case
	    (funcall (solve-function solver) problem timeout)
	  (error (c)
	    (make-instance 'eprover-result
			   :text (format nil "Internal Common Lisp error:~%~a" c)
			   :szs-status (lookup-szs-status "Error")))))
    (timeout-error (c)
      (declare (ignore c))
      (make-instance 'eprover-result
		     :text ""
		     :szs-status (lookup-szs-status "Timeout")))
    (error (c)
      (make-instance 'eprover-result
		     :text (format nil "Internal Common Lisp error:~%~a" c)
		     :szs-status (lookup-szs-status "Error")))))

(defmethod solve (solver (problem pathname) &key (timeout +default-timeout+))
  (solve solver (read-tptp problem) :timeout timeout))
