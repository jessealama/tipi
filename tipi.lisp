
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
    (group (:header "String options:")
	   (stropt :short-name "n" :long-name "name"
		   :description "Set your name to NAME."
		   :argument-name "NAME"))
    (group (:header "Lisp objects:")
	   (lispobj :short-name "e" :long-name "eval"
		    :description "Evaluate EXPR."
		    :argument-name "EXPR"))
    (group (:header "Enumerations:")
	   (enum :long-name "copyright"
		 :description "Set the copyright to LICENSE.
Possible values are: none, gpl, lppl, bsd or mit."
		 :argument-name "LICENSE"
		 :argument-type :optional
		 :enum '(:none :gpl :lppl :bsd :mit)
		 :fallback-value :gpl
		 :default-value :none))
    (group (:header "Path options:")
      (path :long-name "tmpdir"
	    :description "Set the temporary directory to DIR."
	    :argument-name "DIR"
	    :type :directory
	    :default-value #p"/tmp/")
      (path :short-name "o" :long-name "output"
	    :description "Output to FILE."
	    :argument-name "FILE"
	    :type :file
	    :default-value #p"a.out")
      (path :short-name "I"
	    :description "Set the include search path to SEARCH-PATH.
SEARCH-PATH is a colon-separated list of directories. Use an empty argument to
remove all search paths."
	    :argument-name "SEARCH-PATH"
	    :type :directory-list
	    :default-value '(#p"/usr/local/share/" #p"/usr/share/")))
    (group (:header "Switches:")
      (switch :short-name "d" :long-name "debug"
	      :description "Turn debugging on or off."
	      :argument-style :on/off
	      :env-var "DEBUG"))
    (group (:header "Extended switches:")
      (xswitch :short-name "c" :long-name "connect"
	       :description "Connect to server.
Possible values are yes, no or try. If try, no errors are reported."
	       :enum '(:try)))))

(defun main ()
  "Entry point for the standalone application."
  (clon:make-context)
  (cond ((clon:getopt :short-name "h")
	 (clon:help))
	((rest (clon:remainder))
	 (clon:help))
	((clon:remainder)
	 (let ((tptp-file (first (clon:remainder))))
	   (let ((minimizations (tipi:minimize (pathname tptp-file))))
	     (if minimizations
		 (if (rest minimizations)
		     (let ((all-premises (make-hash-table :test #'equal))
			   (common-premises nil))
		       (dolist (minimization minimizations)
			 (dolist (premise minimization)
			   (setf (gethash (tipi::name premise) all-premises) 0)))
		       (dolist (premise (tipi::hash-table-keys all-premises))
			 (when (every #'(lambda (minimization)
					  (member premise minimization
						  :test #'string=
						  :key #'tipi::name))
				      minimizations)
			   (push premise common-premises)))
		       (setf common-premises (sort common-premises #'string<))
		       (format t "Premises common to every minimal solution:~%")
		       (if common-premises
			   (format t "~{~a~%~}" common-premises)
			   (format t "(none)"))
		       (loop
			  for i from 1 upto (length minimizations)
			  for minimization in minimizations
			  do
			    (format t "~%Minimization ~d:~%" i)
			    (let ((sorted-minimization (sort (mapcar #'tipi::name minimization) #'string<)))
			      (dolist (premise sorted-minimization)
				(unless (member premise common-premises :test #'string=)
				  (format t "~a~%" premise))))))
		     (let ((minimization (first minimizations)))
		       (setf minimization (sort (mapcar #'tipi::name minimization) #'string<))
		       (format t "~%Minimization 1:~%")
		       (format t "~{~a~%~}~%" minimization)))
		 (format t "It seems that something went wrong: we found no minimizations at all.~%")))))
	(t
	 (clon:help)))
  (clon:exit))

(clon:dump "tipi" main)
