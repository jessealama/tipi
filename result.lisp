
(in-package :tipi)

(defclass result ()
  ((text
    :type string
    :initarg :text
    :initform (error "To specify a result, text must be supplied.")
    :accessor text)))

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

(defgeneric szs-status (result))

(defmethod szs-status :around ((result result))
  (let ((status (call-next-method)))
    (or status
	(error "We failed to extract an SZS status from the results~%~%~a~%" (text result)))))

(defmethod szs-status ((result result))
  (let ((text (text result)))
    (register-groups-bind (status)
	("SZS status ([A-Za-z]+)" text)
      (lookup-szs-status status))))
