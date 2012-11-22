
(in-package :tipi)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Running programs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-program (program args &key search input output error wait if-output-exists)
  (declare (ignorable search))
  #+sbcl
  (sb-ext:run-program program
		      args
		      :search t
		      :input input
		      :output output
		      :error error
		      :wait wait
		      :if-output-exists if-output-exists)
  #+ccl
  (ccl:run-program program
		   args
		   :input input
		   :output output
		   :error error
		   :wait wait
		   :if-output-exists if-output-exists))

(defun process-exit-code (process)
  #+sbcl
  (sb-ext:process-exit-code process)
  #+ccl
  (multiple-value-bind (status exit-code)
      (ccl:external-process-status process)
    (declare (ignore status))
    exit-code))

(defun process-status (process)
  #+sbcl
  (sb-ext:process-status process)
  #+ccl
  (multiple-value-bind (status exit-code)
      (ccl:external-process-status process)
    (declare (ignore exit-code))
    status))

(defun process-output (process)
  #+sbcl
  (sb-ext:process-output process)
  #+ccl
  (ccl:external-process-output-stream process))

(defun process-error (process)
  #+sbcl
  (sb-ext:process-error process)
  #+ccl
  (ccl:external-process-error-stream process))

(defun process-kill (process &optional (signal 1))
  #+ccl (ccl:signal-external-process process signal)
  #+sbcl (progn
	   (sb-ext:process-close process)
	   (sb-ext:process-kill process signal))
  #-(or sbcl ccl) (error "We handle only SBCL and CCL."))
