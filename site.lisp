
(in-package :tipi)

(define-constant +tipi-port+ 5678
  :test #'=
  :documentation "The port on which the Tipi server listens.")

(defclass tipi-acceptor (hunchentoot:acceptor)
  ())

(defparameter *parser-acceptor* nil
  "The Hunchentoot acceptor that we use for the parser service.")

(defun start-tipi-server ()
  (let ((acceptor (make-instance 'tipi-acceptor :port +tipi-port+)))
    (setf *tipi-hunchentoot-acceptor* acceptor)
    (hunchentoot:start *tipi-hunchentoot-acceptor*)))

(defun stop-tipi-server ()
  (when (null *tipi-hunchentoot-acceptor*)
    (error "The server appears not to be running."))
  (hunchentoot:stop *tipi-hunchentoot-acceptor*))

(define-constant +information-message+
    (with-html-output-to-string (dummy)
      (:head
       (:title "tipi")
       (:link :rel "stylesheet" :href "tipi.css" :type "text/css" :media "screen")
       (:link :rel "icon" :href "favicon.ico" :type "image/png"))
      (:body
       (:h1 "About this service")
       (:p "Where would you like your interets delivered?")))
  :test #'string=
  :documentation "The informational message that we output when we detect that information is what is sought (as opposed to detecting that a parser operation should be carried out).")

(defmacro emit-canned-message ()
  `(progn
     (setf (content-type*) "application/xhtml+xml")
     (with-html (:doctype "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">")
       (str +information-message+))))

(defmethod acceptor-dispatch-request ((acceptor tipi-acceptor) request)
  (let ((uri (script-name request)))
    (if (string= uri "/")
	(emit-canned-message)
	(return-message +http-not-found+
			:mime-type "text/plain"
			:message (format nil "Unknown resource ~a." uri)))))

(defmacro return-message (http-return-code &key (message "")
					        (mime-type nil))
  `(progn
     (setf (return-code*) ,http-return-code
	   (content-type*) ,mime-type)
     (setf (header-out "Server") nil)
     (setf (header-out "Vary") nil)
     ,message))
