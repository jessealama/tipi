
(in-package :tipi)

(defclass tstp-db (tptp-db)
  ((problem
    :type tptp-db
    :accessor problem
    :initarg :problem)))

(defgeneric read-tstp (tptp-thing source-problem))

(defmethod read-tstp :around ((tptp-thing pathname) source-problem)
  (if (probe-file tptp-thing)
      (call-next-method)
      (error "There is no file at '~a'." (namestring tptp-thing))))

(defmethod read-tstp ((tptp-file pathname) source-problem)
  (let ((lisp-string (apply-stylesheet *tptp-to-lisp-stylesheet*
				       (xmlize-tptp tptp-file)
				       nil
				       nil))
	(solution (make-instance 'tstp-db
				:path tptp-file)))
    (with-readtable (find-readtable :modern)
      (let ((tptp-form (handler-case (read-from-string lisp-string)
			 (error (c) (error "Unable to make sense of~%~%~a~%~%as a Lisp representation of~%~%  ~a~%~%The error was:~%~%  ~a" lisp-string (namestring tptp-file) c)))))
	(setf (formulas solution) (mapcar #'make-tptp-formula tptp-form))))
    (setf (problem solution) (read-tptp source-problem))
    solution))

(defmethod read-tstp ((tptp-string string) source-problem)
  (let ((temp (temporary-file)))
    (with-open-file (tptp-file temp
			       :direction :output
			       :if-does-not-exist :create
			       :if-exists :supersede)
      (format tptp-file "~a" tptp-string))
    (read-tstp temp source-problem)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filtering solutions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric restrict-solution-to-problem-language (tstp)
  (:documentation "Restrict TSTP to the language employed by its underlying problem.  The main application is to eexclude Skolem functions and splitting predicates that are present in the solution but not in the problem."))

(defmethod restrict-solution-to-problem-language ((tstp tstp-db))
  (loop
     with problem = (problem tstp)
     with problem-signature = (signature problem)
     for formula in (formulas tstp)
     when (belongs-to-signature formula problem-signature)
       collect formula into filtered-formulas
     finally
       (return (make-instance 'tptp-db
			      :formulas filtered-formulas))))
