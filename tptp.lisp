
(in-package :tipi)

(defparameter *tptp-to-lisp-stylesheet*
  #p"/Users/alama/sources/xsl4tptp/tptp-to-lisp.xsl")

(defgeneric xmlize-tptp (tptp))

(defmethod xmlize-tptp :around ((tptp-file pathname))
  (if (probe-file tptp-file)
      (call-next-method)
      (error "There is no file at '~a'." (namestring tptp-file))))

(defmethod xmlize-tptp ((tptp-file pathname))
  (let ((tptp-process (run-program "tptp4X"
				   (list "-c" "-x" "-fxml" (namestring tptp-file))
				   :search t
				   :wait t
				   :output :stream
				   :input nil)))
    (let ((output-stream (process-output tptp-process)))
      (let ((output-lines (stream-lines output-stream)))
	(format nil "~{~a~%~}" output-lines)))))

(defgeneric read-tptp (tptp-thing))

(defmethod read-tptp :around ((tptp-thing pathname))
  (if (probe-file tptp-thing)
      (call-next-method)
      (error "There is no file at '~a'." (namestring tptp-thing))))

(defmethod read-tptp ((tptp-file pathname))
  (let ((lisp-string (apply-stylesheet *tptp-to-lisp-stylesheet*
				       (xmlize-tptp tptp-file)
				       nil
				       nil)))
    (with-readtable (find-readtable :modern)
      (let ((tptp-form (handler-case (read-from-string lisp-string)
			 (error (c) (error "Unable to make sense of~%~%~a~%~%as a Lisp representation of~%~%  ~a~%~%The error was:~%~%  ~a" lisp-string (namestring tptp-file) c)))))
	(let ((problem (make-instance 'tptp-problem)))
	  (setf (formulas problem)
		(mapcar #'make-tptp-formula tptp-form)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass tptp-problem ()
  ((formulas :type list
	     :accessor formulas)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass tptp-formula ()
  ((name :type string
	 :initarg :name)
   (syntax :type string
	   :initarg :syntax)
   (status :type string
	   :initarg :status)
   (formula :type formula
	    :initarg :formula)
   (source :type list
	   :initarg source)
   (useful-info :type list
		:initarg useful-info)))

(defgeneric make-tptp-formula (thing))

(defmethod make-tptp-formula ((thing list))
  (destructuring-bind (syntax name status formula . more-stuff)
      thing
    (declare (ignore more-stuff))
    (make-instance 'tptp-formula
		   :name (symbol-name name)
		   :syntax (symbol-name syntax)
		   :status (symbol-name status)
		   :formula (form->formula formula))))
