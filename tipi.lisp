
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
	"Tipi -- A TPTP-based toolkit for proof analysis")
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

(defun file-readable? (path)
  (and (probe-file path)
       (streamp (handler-case (open path :direction :probe)
		  (error () nil)))))

(defun red (str)
  (format nil "~C[;31m~a~C[0;m" #\Escape str #\Escape))

(defun error-message (format-string &rest format-args)
  (format *error-output* (red "Error"))
  (format *error-output* "~a" #\Space)
  (apply #'format *error-output* format-string format-args))

(defmacro help-and-die ()
  `(progn
     (clon:help)
     (clon:exit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Needed premises
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric needed (problem timeout))

(defmethod needed ((problem null) timeout)
  (declare (ignore timeout))
  (help-and-die))

(defmethod needed ((problem list) timeout)
  (if (rest problem)
      (help-and-die)
      (needed (first problem) timeout)))

(defmethod needed ((problem string) timeout)
  (needed (pathname problem) timeout))

(defmethod needed :around ((problem pathname) timeout)
  (declare (ignore timeout))
  (cond ((file-readable? problem)
	 (handler-case (call-next-method)
	   (error (err)
	     (error-message "Something went wrong while computing needed premises:~%~%~a~%~%Please inform the maintainers.  Sorry." err)
	     (clon:exit 1))))
	(t
	 (error-message "There is no file at~%~%  ~a~%~%or it is unreadable.~%" (namestring problem))
	 (clon:exit 1))))

(defmethod needed ((problem pathname) timeout)
  (destructuring-bind (needed unneeded unknown)
      (tipi::needed-premises problem :timeout timeout)
    (let ((needed-sorted (tipi::sort-formula-list needed))
	  (needed-sorted (tipi::sort-formula-list unneeded))
	  (unknown-sorted (tipi::sort-formula-list unknown)))
      (if needed-sorted
	  (format t "The following premises are needed:~%~%~{~a~%~}" needed-sorted)
	  (format t "No premise was shown to be needed."))
      (terpri)
      (if unneeded-sorted
	  (format t "The following premises are not needed:~%~%~{~a~%~}" unneeded-sorted)
	  (format t "No premise was shown to be unneeded."))
      (if unknown-sorted
	  (format t "We were unable to determine whether the following premises are needed:~%~%~{~a~%~}" unknown-sorted)
	  (format t "We were able to make a decision about every premise.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minimize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric minimize (problem timeout))

(defmethod minimize ((problem string) timeout)
  (minimize (pathname problem) timeout))

(defmethod minimize ((problem null) timeout)
  (declare (ignore timeout))
  (help-and-die))

(defmethod minimize ((problem list) timeout)
  (if (rest problem)
      (help-and-die)
      (minimize (first problem) timeout)))

(defmethod minimize :around ((problem pathname) timeout)
  (declare (ignore timeout))
  (cond ((file-readable? problem)
	 (handler-case (call-next-method)
	   (error (err)
	     (error-message "Something went wrong during minimization:~%~%~a~%~%Please inform the maintainers.  Sorry." err)
	     (clon:exit 1))))
	(t
	 (error-message "There is no file at~%~%  ~a~%~%or it is unreadable.~%" (namestring problem))
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
  (when (clon:getopt :short-name "h")
    (help-and-die))
  (unless (clon:remainder)
    (help-and-die))
  (let ((timeout-str (clon:getopt :long-name "timeout"))
	(timeout nil)
	(remainder (clon:remainder)))
    (handler-case (setf timeout (parse-integer timeout-str
					       :junk-allowed nil))
      (error ()
	(error-message "'~a' is not an acceptable value for the timeout option." timeout-str)
	(clon:exit 1)))
    (let ((command (first remainder))
	  (remainder (rest remainder)))
      (cond ((string= command "needed")
	     (needed remainder timeout))
	    ((string= command "minimize")
	     (minimize remainder timeout))
	    (t
	     (help-and-die)))))
  (clon:exit))

(clon:dump "tipi" main)
