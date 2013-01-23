
(in-package :tipi)

(define-constant +tipi-port+ 5678
  :test #'=
  :documentation "The port on which the Tipi server listens.")

(defmacro return-message (http-return-code &key (message "")
					        (mime-type nil))
  `(progn
     (setf (return-code*) ,http-return-code
	   (content-type*) ,mime-type)
     (setf (header-out "Server") nil)
     (setf (header-out "Vary") nil)
     ,message))

(defclass tipi-acceptor (hunchentoot:acceptor)
  ())

(defparameter *tipi-acceptor* nil
  "The Hunchentoot acceptor that we use for the parser service.")

(defun start-tipi-server ()
  (let ((acceptor (make-instance 'tipi-acceptor :port +tipi-port+)))
    (setf *tipi-acceptor* acceptor)
    (hunchentoot:start *tipi-acceptor*)))

(defun stop-tipi-server ()
  (when (null *tipi-acceptor*)
    (error "The server appears not to be running."))
  (hunchentoot:stop *tipi-acceptor*))

(defparameter *information-message*
  (with-html-output-to-string (dummy)
    (:head
     (:title "tipi")
     (:link :rel "stylesheet" :href "tipi.css" :type "text/css" :media "screen")
     (:link :rel "icon" :href "favicon.ico" :type "image/png"))
    (:body
     (:h1 "About this service")
     (:p "Where would you like your interets delivered?")))
  "The informational message that we output when we detect that information is what is sought (as opposed to detecting that a parser operation should be carried out).")

(defparameter *main-page*
  (with-html-output-to-string (dummy)
    (:head
     (:title "tipi")
     (:link :rel "stylesheet" :href "tipi.css" :type "text/css" :media "screen")
     (:link :rel "icon" :href "favicon.ico" :type "image/png"))
    (:body
     (:h1 "Analyze your problems and their solutions")
     ((:div :id "main-forms")
      ((:ul :id "main-form-list")
       (:li ((:a :href "#analyze-by-uri") "Analyze by URI"))
       (:li ((:a :href "#analyze-by-upload") "Analyze by File Upload"))
       (:li ((:a :href "#analyze-by-input") "Analyze by Direct Input"))))
     ((:div :id "fields")
      ((:fieldset :id "analyze-by-uri" :class "tabset-content front")
       ((:legend :class "tabset-label") "Analyze by URI")
       ((:form
	 :id "tipi-form"
	 :title "Submit your TPTP problem/TSTP solution for analysis"
	 :action "analyze"
	 :method "get")
	((:p :class "instructions")
	 "Analyze a TPTP problem or TSTP solution that can be fetched via the Internet."
	 ((:label :title "Address of problem or solution"
		  :for "uri")
	  "Address:")
	 ((:input
	   :type "text"
	   :id "uri"
	   :size "50")))
	((:p :class "submit-button")
	 ((:input :type "submit"
		  :title "Submit for analysis"
		  :value "Analyze")))))
      ((:fieldset
	:id "analyze-by-upload"
	:class "tabset-content front")
       ((:legend :class "tabset-label")
	"Analyze by File Upload")
       ((:form
	 :method "post"
	 :enctype "multipart/form-data"
	 :action "analyze")
	((:p :class "instructions")
	 "Upload a problem or solution for validation")
	((:p)
	 ((:label :title "Choose a file"
		  :for "uploaded-file"))
	 ((:input :type "file"
		  :id "uploaded-file"
		  :size "30")))
	((:p :class "submit-button")
	 ((:input :title "Submit for analysis"
		  :type "submit"
		  :value "Analyze")))))
      ((:fieldset
	:id "analyze-by-input"
	:class "tabset-content front")
       ((:legend :class "tabset-label") "Analyze by direct input")
       ((:form
	 :method "post"
	 :enctype "multipart/form-data"
	 :action "analyze")
	((:p :class "instructions")
	 ((:label :title "Paste a problem or solution here") "Enter the text of the TPTP/TSTP file to be analyzed")
	 ":"
	 (:br)
	 ((:textarea
	   :id "fragment"
	   :name "fragment"
	   :rows "12"
	   :cols "80")))
	((:p :class "submit-button")
	 ((:input :title "Submit for analysis"
		  :type "submit"
		  :value "Analyze"))))))
     ((:div :class "intro" :id "documentation")
      ((:ul :class "navigation-bar")
       (:li ((:a :href "about" :title "Information about this service") "About"))))
     ((:div :id "footer")
      ((:p :class "logo")
       ((:a :href "http://www.tptp.org")
	((:img :src "tptp.png" :alt "The TPTP logo" :title "TPTP"))))))))

(defmacro emit-canned-message ()
  `(progn
     (setf (content-type*) "application/xhtml+xml")
     (with-html (:doctype "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">")
       (str *information-message*))))

(defparameter *xhtml-11-doctype*
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">")

(defmacro emit-xhtml (html)
  `(progn
     (setf (content-type*) "application/xhtml+xml")
     (with-html (:doctype *xhtml-11-doctype*)
       (str ,html))))

(defgeneric handle-/ (request method session))

(defmethod handle-/ (request (method (eql :get)) session)
  (return-message +http-ok+
		  :message (emit-xhtml *main-page*)
		  :mime-type "application/xhtml+xml"))

(defmethod handle-/ (request (method t) session)
  (return-message +http-method-not-allowed+
		      :message (format nil "Inappropriate method ~a for resource '/'." method)
		      :mime-type "text/plain"))

(defgeneric handle-/expand (request method session))

(defmethod handle-/expand (request (method (eql :post)) session)
  (multiple-value-bind (problem problem-known-p)
      (session-value :problem session)
    (if problem-known-p
	(let* ((includes (include-instructions problem))
	       (included-files (mapcar #'file includes)))
	  (when (null includes)
	    (redirect "/analyze" :add-session-id t))
	  (setf included-files
		(mapcar #'namestring
			(remove-duplicates included-files :test #'string= :key #'namestring)))
	  (setf (session-value :missing-includes session) included-files)
	  (emit-xhtml
	   (with-html-output-to-string (dummy)
	     (:head (:title "gimme"))
	     (:p
	      "Please supply the following files:")
	     ((:form :method "post"
		     :action "upload-includes"
		     :enctype "multipart/form-data")
	      (:ul
	       (loop
		  :for file :in included-files
		  :for i from 1
		  :do
		  (htm (:li (:p (fmt "~a" file))
			    (:textarea :id (format nil "include-~d" i)
				       :name (format nil "include-~d" i))))))
	      ((:input :type "submit"
		       :title "Upload missing TPTP files"
		       :value "Upload")))))))))

(defmethod handle-/expand (request (method t) session)
  (return-message +http-method-not-allowed+))

(defgeneric handle-/upload-includes (request method session))

(defmethod handle-/upload-includes (request (method (eql :post)) session)
  (let ((parameters (post-parameters request)))
    (multiple-value-bind (problem problem-known-p)
	(session-value :problem session)
      (if problem-known-p
	  (multiple-value-bind (missing-includes includes-known-p)
	      (session-value :missing-includes session)
	    (if includes-known-p
		(if (null missing-includes)
		    (redirect "/analyze" :add-session-id t)
		    (if (every #'(lambda (n)
				   (assoc (format nil "include-~d" n) parameters :test #'string=))
			       (first-n-numbers (length missing-includes)))
			(let ((include-table (make-hash-table :test #'equal))
			      (error-message nil))
			  (loop
			     :for i :from 1
			     :for filename :in missing-includes
			     :for contents = (cdr (assoc (format nil "include-~d" i) parameters :test #'string=))
			     :for db = (handler-case (parse-tptp contents)
			  		 (error (e)
			  		   (setf error-message (format nil "~a" e))
			  		   nil))
			     :do
			     (unless db
			       (return-from handle-/upload-includes
				   (return-message +http-bad-request+
						   :message (format nil "wtfdude, parse error (~a)" error-message)
						   :mime-type "text/plain")))
			     (setf (gethash (format nil "~a" filename) include-table) db))
			  (let ((includes (include-instructions problem))
			  	(non-includes (formulas-w/o-includes problem))
			  	(new-formulas nil))
			    (dolist (include includes)
			      (let ((file (file include))
			  	    (selection (selection include)))
			  	(setf file (format nil "~a" file))
			  	(multiple-value-bind (db-for-file present?)
			  	    (gethash file include-table)
			  	  (if present?
			  	      (dolist (selected-name selection)
			  		(let ((new-formula (formula-with-name db-for-file selected-name)))
			  		  (if new-formula
			  		      (push new-formula new-formulas)
			  		      (emit-xhtml
			  		       (with-html-output-to-string (dummy)
			  			 (:head (:title "no such formula"))
			  			 (:body
			  			  (:p (fmt "no known formula in '~a' with the name '~a'" file selected-name))))))))
			  	      (emit-xhtml
			  	       (with-html-output-to-string (dummy)
			  		 (:head (:title "no such include"))
			  		 (:body
			  		  (:p (fmt "no known include with the name '~a'" file)))))))))
			    (setf new-formulas (reverse new-formulas))
			    (setf (session-value :fuck session) "you")
			    (setf (session-value :problem session)
			  	  (make-instance 'tptp-db
			  			 :formulas (append new-formulas
			  					   non-includes))))
			  (redirect "/analyze" :add-session-id t))
			(emit-xhtml
			 (with-html-output-to-string (dummy)
			   (:body
			    (:p "every is false.  parameters: " (fmt "~a" parameters)))))
			))
		(redirect "/analyze" :add-session-id t)))
	  (emit-xhtml
	   (with-html-output-to-string (dummy)
	     (:body
	      (:p "problem is not known"))))))))

(defmethod handle-/upload-includes (request (method t) session)
  (return-message +http-method-not-allowed+))

(defgeneric handle-/analyze (request method session))

(defmethod handle-/analyze (request (method (eql :post)) session)
  (let* ((parameters (post-parameters request))
	 (fragment (assoc "fragment" parameters
			  :test #'string=))
	 (error-message nil))
    (if fragment
	(let ((db (handler-case (parse-tptp (cdr fragment))
		    (error (c)
		      (setf error-message (format nil "~a" c))
		      nil))))
	  (if db
	      (progn
		(setf (session-value :problem session) db)
		(redirect "/analyze" :add-session-id t))
	      (return-message +http-bad-request+
			      :message (emit-xhtml
					(with-html-output-to-string (dummy)
					  (:head
					   (:link :rel "stylesheet" :href "tipi.css" :type "text/css" :media "screen")
					   (:link :rel "icon" :href "favicon.ico" :type "image/png")
					   (:title "something failed"))
					  (:body
					   (:p "Something went wrong.  It's possible that your TPTP/TSTP file is malformed, or the TPTP parser implemented by this site may be flawed. Here is the error message:")
					   (:pre (fmt "~a" (escape-string error-message))))))
			      :mime-type "application/xhtml+xml")))
	(return-message +http-internal-server-error+
		    :message "Don't know how to handle a POST request that lacks a value for 'fragment'.  (Functionality may not yet be implemented.)"
		    :mime-type "text/plain"))))

(defmethod handle-/analyze (request (method (eql :get)) session)
  (declare (ignore request))
  (multiple-value-bind (old-problem problem-known-p)
      (session-value :problem session)
    (if problem-known-p
	(return-message +http-ok+
			:message (emit-xhtml
				  (with-html-output-to-string (dummy)
				    (:head
				     (:link :rel "stylesheet" :href "tipi.css" :type "text/css" :media "screen")
				     (:link :rel "icon" :href "favicon.ico" :type "image/png")
				     (:title "parseable!"))
				    (:body
				     (fmt "~a" (render-html old-problem)))))
			:mime-type "application/xhtml+xml")
	(redirect "/" :add-session-id t))))

(defmethod acceptor-dispatch-request ((acceptor tipi-acceptor) request)
  (let ((uri (script-name request))
	(method (request-method*))
	(session *session*))
    (cond ((string= uri "/tptp.png")
	   (handle-static-file "tptp.png" "image/png"))
	  ((string= uri "/tipi.css")
	   (handle-static-file #p"/Users/alama/sources/tipi/tipi.css"
			  "text/css"))
	  ((string= uri "/")
	   (handle-/ request method session))
	  ((string= uri "/expand")
	   (handle-/expand request method session))
	  ((string= uri "/upload-includes")
	   (handle-/upload-includes request method session))
	  ((string= uri "/analyze")
	   (handle-/analyze request method session))
	  (t
	   (return-message +http-not-found+
			   :message "Unknown resource")))))
