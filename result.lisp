
(in-package :tipi)

(defclass result ()
  ((text
    :type string
    :initarg :text
    :initform (error "To specify a result, text must be supplied.")
    :accessor text)
   (szs-status
    :type szs-status
    :initarg :szs-status
    :initform (lookup-szs-status "Unknown")
    :accessor szs-status)))

(defmethod initialize-instance :after ((result result) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (let ((text (text result)))
    (let ((current-szs-status (szs-status result)))
      (when (eql current-szs-status (lookup-szs-status "Unknown"))
	(let ((extracted-status (register-groups-bind (status)
				    ("SZS status ([A-Za-z]+)" text)
				  (lookup-szs-status status))))
	  (if extracted-status
	      (setf (szs-status result) extracted-status))))))
  result)

(defmethod print-object ((result result) stream)
  (print-unreadable-object (result stream :type t :identity nil)
    (format stream "SZS Status: ~a" (szs-status result))
    (terpri stream)
    (let ((text (text result)))
      (if (string= text "")
	  (format stream "Text: (none)")
	  (format stream "Text:~%~a" text)))))

(defgeneric interpret (result))

(defclass eprover-result (result)
  nil)

(defclass paradox-result (result)
  nil)

(defmethod interpret ((result eprover-result))
  (read-tptp (text result)))

(defun filter-paradox-text (text)
  "The non-TPTP header and footer lines of TEXT."
  (let ((model-lines
	 (loop
	    with lines = nil
	    with inside-model? = nil
	    for line in (split-lines text)
	    do
	      (cond ((scan "SZS output end FiniteModel" line)
		     (return (reverse lines)))
		    (inside-model?
		     (push line lines))
		    ((scan "SZS output start FiniteModel" line)
		     (setf inside-model? t))))))
    (format nil "~{~a~%~}" model-lines)))

(defmethod interpret ((result paradox-result))
  (read-tptp (filter-paradox-text (text result))))
