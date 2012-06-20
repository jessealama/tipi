
(in-package :tipi)

(defgeneric apply-stylesheet (stylesheet xml-document parameters output)
  (:documentation "Apply the XSL stylesheet STYLESHEET to XML-DOCUMENT  OUTPUT is either NIL or a string or a pathname; NIL means that the value of APPLY-STYLESHEET will be the output of the XSLT processor as a string.  If OUTPUT is either a string or a pathname, it will be interpreted as the path to a file where the output should be saved.  If there is already a file at the supplied OUTPUT, it will be overwritten.  If the value of OUTPUT is either a string or a pathname, the value of the function will be simply T.  The XSLT processor may signals an error during the application of STYLESHEET to XML-DOCUMENT.  PARAMETERS is supposed to be an association list mapping strings to strings."))

(defmethod apply-stylesheet ((stylesheet string) xml-document parameters output)
  "Apply the stylesheet indicated by STYLESHEET to XML-DOCUMENT, saving the result in OUTPUT.  (If OUTPUT is NIL, the value of this function will be a string, representing the result of applying STYLESHEET to XML-DOCUMENT.  Otherwise, OUTPUT will be interpreted as a file, and the result of applying the XSLT processor will be stored there.

If STYLESHEET is the empty string, nothing will be done, and XML-DOCUMENT will be returned.  If STYLESHEET is not the empty string, its first character will be consulted.  If it is a forward slash '/', then STYLESHEET will be understood as a path to an XSL file.  If STYLESHEET is not empty and its first character is not a forward slash '/', then STYLESHEET will be understood as a string representation of an XSL stylesheet.  PARAMETERS is suposed to be an association list mapping strings to strings."
  (if (string= stylesheet "")
      xml-document
      (let ((first-char (char stylesheet 0)))
	(if (char= first-char #\/)
	    (apply-stylesheet (pathname stylesheet) xml-document parameters output)
	    (let ((tmp-xsl-path (temporary-file)))
	      (with-open-file (xsl tmp-xsl-path
				   :direction :output
				   :if-exists :error
				   :if-does-not-exist :create)
		(format xsl "~a" stylesheet))
	      (prog1
		  (apply-stylesheet tmp-xsl-path xml-document parameters output)
		(delete-file tmp-xsl-path)))))))

(defmethod apply-stylesheet (stylesheet (xml-document string) parameters output)
  "Apply the stylesheet indicated by STYLESHEET to XML-DOCUMENT, saving the result in OUTPUT.  (If OUTPUT is NIL, the value of this function will be a string, representing the result of applying STYLESHEET to XML-DOCUMENT.  Otherwise, OUTPUT will be interpreted as a file, and the result of applying the XSLT processor will be stored there.

If XML-DOCUMENT is the empty string, nothing will be done, and XML-DOCUMENT (viz, the empty string) will be returned.  If XML-DOCUMENT is not the empty string, its first character will be consulted.  If it is a forward slash '/', then XML-DOCUMENT will be understood as a path to an XML file.  If XML-DOCUMENT is not empty and its first character is not a forward slash '/', then XML-DOCUMENT will be understood as a string representation of an XML document.  PARAMETERS is supposed to be an association list mapping strings to strings."
  (if (string= xml-document "")
      xml-document
      (let ((first-char (char xml-document 0)))
	(if (char= first-char #\/)
	    (apply-stylesheet stylesheet (pathname xml-document) parameters output)
	    (let ((tmp-xml-path (temporary-file)))
	      (with-open-file (xml tmp-xml-path
				   :direction :output
				   :if-exists :error
				   :if-does-not-exist :create)
		(format xml "~a" xml-document))
	      (prog1
		  (apply-stylesheet stylesheet tmp-xml-path parameters output)
		(delete-file tmp-xml-path)))))))

(defmethod apply-stylesheet (stylesheet (xml-document stream) parameters output)
  "Apply the stylesheet indicated by STYLESHEET to XML-DOCUMENT, saving the result in OUTPUT.  (If OUTPUT is NIL, the value of this function will be a string, representing the result of applying STYLESHEET to XML-DOCUMENT.  Otherwise, OUTPUT will be interpreted as a file, and the result of applying the XSLT processor will be stored there."
  (apply-stylesheet stylesheet
		    (format nil "~{~a~%~}" (stream-lines xml-document))
		    parameters
		    output))

(defmethod apply-stylesheet :around ((stylesheet pathname) xml-document parameters output)
  (declare (ignore xml-document output parameters))
  (if (file-exists-p stylesheet)
      (call-next-method)
      (error "There is no stylesheet at ~a" (namestring stylesheet))))

(defmethod apply-stylesheet :around (stylesheet (xml-document pathname) parameters output)
  (declare (ignore stylesheet output parameters))
  (if (file-exists-p xml-document)
      (call-next-method)
      (error "There is no XML document at ~a" (namestring xml-document))))

(defmethod apply-stylesheet :around (stylesheet xml-document parameters output)
  (declare (ignore stylesheet xml-document output))
  (if (listp parameters)
      (flet ((string-string-cons (x)
	       (and (consp x)
		    (stringp (car x))
		    (stringp (cdr x)))))
	(if (every #'string-string-cons parameters)
	    (call-next-method)
	    (error "The supplied list of parameters is not an association list that maps strings to strings!")))
      (error "The supplied parameter 'list' isnt' actually a list")))

(defmethod apply-stylesheet :around (stylesheet xml-document parameters (output string))
  (declare (ignore stylesheet xml-document parameters))
  (let ((writeable t))
    (handler-case (ensure-directories-exist output)
      (error () (setf writeable nil)))
    (if writeable
	(call-next-method)
	(error "Cannot save output to '~a' because we cannot ensure that its directories exist" output))))

(defmethod apply-stylesheet :around (stylesheet xml-document parameters (output pathname))
  (declare (ignore stylesheet xml-document parameters))
  (let ((writeable t))
    (handler-case (ensure-directories-exist output)
      (error () (setf writeable nil)))
    (if writeable
	(call-next-method)
	(error "Cannot save output to '~a' because we cannot ensure that its directories exist" (namestring output)))))

(defmethod apply-stylesheet ((stylesheet pathname) (xml-document pathname) parameters output)
  (labels ((xsltproc-args ()
	     (loop
		with args = nil
		for (param . value) in parameters
		do
		  (push value args)
		  (push param args)
		  (push "--stringparam" args)
		finally
		  (return args))))
    (let* ((stylesheet-name (namestring stylesheet))
	   (document-name (namestring xml-document))
	   (xsltproc (run-program "xsltproc"
				  (append (xsltproc-args)
					  (list stylesheet-name
						document-name))
				  :search t
				  :input nil
				  :output (or output :stream)
				  :if-output-exists :supersede
				  :error :stream
				  :wait nil)))
      (let* ((out (process-output xsltproc))
	     (out-lines (stream-lines out)))
	(let ((exit-code (process-exit-code xsltproc)))
	  (if (or (null exit-code)
		  (zerop exit-code))
	      (if output
		  t
		  (format nil "~{~a~%~}" out-lines))
	      (let* ((err (process-error xsltproc))
		     (err-lines (stream-lines err)))
		(if err-lines
		    (error "xsltproc did not exit cleanly when called on~%~%  ~a~%~%and~%~%  ~a;~%~%the exit code was ~a.~%~%Here is the content of the standard error stream:~%~%~{  ~a~%~}" stylesheet-name document-name exit-code err-lines)
		    (error "xsltproc did not exit cleanly when called on~%~%  ~a~%~%and~%~%  ~a;~%~%the exit code was ~a.~%~%(There was no output on standard error.)" stylesheet-name document-name exit-code)))))))))
