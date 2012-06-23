
(in-package :cl-user)

(require 'asdf)

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(quicklisp:quickload "alexandria")

(asdf:load-system "tipi")

;; Load CLON
(quicklisp:quickload "com.dvlsoft.clon")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (com.dvlsoft.clon:nickname-package))

(clon:defsynopsis (:postfix "FILES...")
  (text :contents
	"Demonstration of Clon (use --clon-help for built-in options).")
  (group (:header "Flags (non valued options):")
    (flag :short-name "h" :long-name "help"
	  :description "Print this help and exit."))
  (group (:header "Built-in valued option types:")
    (group (:header "Limits")
	   (stropt :short-name "t"
		   :long-name "timeout"
		   :description "Spend at most TIME seconds solving any particular problem."
		   :argument-name "TIME"
		   :default-value "5"))))

(defun error-message (format-string &rest format-args)
  (format *error-output* "~C[;31mError~C[0;m" #\Escape #\Escape)
  (format *error-output* "~a" #\Space)
  (apply #'format *error-output* format-string format-args))

(defgeneric minimize (problem timeout))

(defmethod minimize ((problem string) timeout)
  (minimize (pathname problem) timeout))

(defmethod minimize :around ((problem pathname) timeout)
  (declare (ignore timeout))
  (cond ((probe-file problem)
	 (call-next-method))
	(t
	 (error-message "There is no file at~%~%  ~a~%" (namestring problem))
	 (clon:exit 1))))

(defmethod minimize ((problem pathname) timeout)
  (destructuring-bind (solutions non-solutions unknown)
      (tipi:minimize problem :timeout timeout)
    (declare (ignore non-solutions))
    (destructuring-bind (common-premises . unique-solutions)
	(tipi::extract-common-elements #'string= solutions)
      (format t "~d minimal solution(s) found.~%" (length solutions))
      (cond ((rest solutions)
	     (format t "Premises common to every minimal solution:~%")
	     (if common-premises
		 (format t "~{~a~%~}" common-premises)
		 (format t "(none)"))
	     (loop
		for i from 1 upto (length unique-solutions)
		for minimization in unique-solutions
		do
		  (format t "~%Minimization ~d:~%~{~a~%~}" i minimization)))
	    (solutions
	   (format t "~%Minimization 1:~%~{~a~%~}" (first solutions))))
      (terpri)
      (if unknown
	  (if (rest unknown)
	      (format t "There were ~d combinations of premises for which we were unable to come to a decision." (length unknown))
	      (format t "There was 1 combination of premises for which we were unable to come to a decision."))
	  (format t "We were able to make a decision about every subset of premises.")))))

(defun main ()
  "Entry point for the standalone application."
  (clon:make-context)
  (cond ((clon:getopt :short-name "h")
	 (clon:help))
	((rest (clon:remainder))
	 (clon:help))
	((clon:remainder)
	 (let ((timeout-str (clon:getopt :long-name "timeout"))
	       (timeout nil))
	   (handler-case (setf timeout (parse-integer timeout-str
						      :junk-allowed nil))
	     (error () (error-message "'~a' is not an acceptable value for the timeout option." timeout-str)))
	   (let ((tptp-file (first (clon:remainder))))
	     (handler-case (minimize tptp-file timeout)
	       (error (err)
		 (error-message "Something went wrong during minimization:~%~%~a~%~%Please inform the maintainers.  Sorry." err)
		 (clon:exit 1))))))
	(t
	 (clon:help)))
  (clon:exit))

(clon:dump "tipi" main)
