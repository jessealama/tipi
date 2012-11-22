
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
	      (eprover-err (make-string-output-stream))
              (eprover-process (run-program "eproof_ram"
					    (list "--auto"
						  (format nil "--cpu-limit=~d" timeout)
						  "--tstp-format"
						  path)
					    :search t
					    :input nil
					    :output :stream
					    :error :stream
					    :wait t))
              (eprover-exit-code (process-exit-code eprover-process)))
         (close eprover-out)
	 (close eprover-err)
         (if (zerop eprover-exit-code)
	     (make-instance 'eprover-result
			    :text (get-output-stream-string eprover-out))
	     (make-instance 'eprover-result
                            :text ""
                            :szs-status (lookup-szs-status
                                         (if (= eprover-exit-code 6) "Timeout" "Error")))))))))

(defparameter *paradox*
  (make-instance
   'solver
   :name "Paradox"
   :solve-function
   (lambda (problem timeout)
     (block paradox
       (let* ((path (native-namestring (path problem)))
	      (paradox-out (make-string-output-stream))
	      (paradox-err (make-string-output-stream))
              (paradox-process (run-program "paradox"
                                            (list "--tstp"
                                                  "--no-progress"
                                                  "--time" (format nil "~d" timeout)
                                                  path)
                                            :search t
                                            :input nil
                                            :output paradox-out
                                            :error paradox-err
                                            :wait t))
              (paradox-exit-code (process-exit-code paradox-process)))
         (close paradox-out)
	 (close paradox-err)
         (if (zerop paradox-exit-code)
             (make-instance 'paradox-result
                            :text (get-output-stream-string paradox-out))
             (make-instance 'paradox-result
                            :text ""
                            :szs-status (lookup-szs-status "Error"))))))))

(defmethod solve :before (prover problem &key timeout)
  (declare (ignore prover problem))
  (when (null timeout)
    (setf timeout +default-timeout+))
  (unless (integerp timeout)
    (error "Invalid value ~a for the timeout parameter." timeout))
  (when (< timeout 1)
    (error "Invalid value ~a for the timeout parameter." timeout)))

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
  (declare (ignore timeout problem))
  (values nil (lookup-szs-status "Unknown")))

(defmethod solve ((solver-list list) problem &key timeout)
  (when (null timeout)
    (setf timeout +default-timeout+))
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
      (delete-file temp)
      )))

(defmethod solve-problem ((db tptp-db) &key (timeout +default-timeout+))
  (if (has-conjecture-formula? db)
      (solve-problem (make-derivability-problem (formulas db)) :timeout timeout)
      (error "The given TPTP database lacks a conjecture formula; we cannot solve it.")))

(define-constant +granularity+ 5
  :test #'=
  :documentation "The number of slices into which 1 second should be divided.")

(defun run-eprover (path timeout output-stream error-stream)
  (run-program "eproof_ram"
	       (list "--auto"
		     "--tstp-format"
		     (format nil "--cpu-limit=~d" timeout)
		     (native-namestring path))
	       :search t
	       :input nil
	       :output output-stream
	       :error error-stream
	       :wait nil))

(defun run-paradox (path timeout output-stream error-stream)
  (run-program "paradox"
	       (list "--model"
		     "--tstp"
		     "--no-progress"
		     "--time" (format nil "~d" timeout)
		     (native-namestring path))
	       :search t
	       :input nil
	       :output output-stream
	       :error error-stream
	       :wait nil))

(defmethod solve-problem ((problem pathname) &key (timeout +default-timeout+))
  (let ((eprover-out (make-string-output-stream))
	(paradox-out (make-string-output-stream))
	(eprover-err (make-string-output-stream))
	(paradox-err (make-string-output-stream))
	(answer (lookup-szs-status "Unknown")))
    (let ((eprover-process (run-eprover problem timeout eprover-out eprover-err))
	  (paradox-process (run-paradox problem timeout paradox-out paradox-err)))
      (loop
	 with results-table = (make-hash-table :test #'equal)
	 with process-table = (make-hash-table :test #'equal)
	 initially
	   (dolist (solver (list "paradox" "eprover"))
	     (setf (gethash solver results-table)
		   (lookup-szs-status "NotTriedYet")))
	   (setf (gethash "eprover" process-table) eprover-process)
	   (setf (gethash "paradox" process-table) paradox-process)
	 for i from 1 upto (* +granularity+ timeout)
	 for eprover-status = (process-status eprover-process)
	 for paradox-status = (process-status paradox-process)
	 do
	   (sleep (float (/ 1 +granularity+)))
	   (if (not (eql eprover-status :running))
	       (let ((eprover-exit (process-exit-code eprover-process)))
		 (if (zerop eprover-exit)
		     (let ((result (make-instance 'eprover-result
						  :text (get-output-stream-string eprover-out))))
		       (let ((status (szs-status result)))
			 (setf (gethash "eprover" results-table)
			       (or status (lookup-szs-status "Unknown")))))
		     (setf (gethash "eprover" results-table)
			   (lookup-szs-status "Error")))))
	   (if (not (eql paradox-status :running))
	       (let ((paradox-exit (process-exit-code paradox-process)))
		 (if (zerop paradox-exit)
		     (let ((result (make-instance 'paradox-result
						  :text (get-output-stream-string paradox-out))))
		       (let ((status (szs-status result)))
			 (setf (gethash "paradox" results-table)
			       (or status (lookup-szs-status "Unknown")))))
		     (setf (gethash "paradox" results-table)
			   (lookup-szs-status "Error")))))
	   (when (some-theorem? (hash-table-values results-table))
	     (dolist (process (hash-table-values process-table))
	       (process-kill process))
	     (setf answer
		   (aggregate-szs-statuses (hash-table-values results-table)))
	     (return))
	   (when (every #'(lambda (status) (not (eql status :running)))
			(mapcar #'process-status (hash-table-values process-table)))
	     (setf answer
		   (aggregate-szs-statuses (hash-table-values results-table)))
	     (return))
	 finally
	   (dolist (process (hash-table-values process-table))
	     (process-kill process))
	   (setf answer (lookup-szs-status "Timeout"))))
    (close eprover-out)
    (close eprover-err)
    (close paradox-out)
    (close paradox-err)
    answer))

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
