
(in-package :tipi)

(defreadtable tptp-syntax
  (:merge :standard)
  (:macro-char #\[
	       #'(lambda (stream char)
		   (declare (ignore char))
		   (read-delimited-list #\] stream)))
  (:macro-char #\(
	       #'(lambda (stream char)
		   (declare (ignore char))
		   (read-delimited-list #\) stream))))

(defgeneric read-tptp (tptp-thing))

(defmethod read-tptp ((tptp-thing string))
  (in-readtable tptp-syntax)
  (read tptp-thing))

(defmethod read-tptp :around ((tptp-file pathname))
  (if (probe-file tptp-file)
      (call-next-method)
      (error "There is no file at '~a'." (namestring tptp-file))))

(defgeneric file-contents (file-thing))

(defun concat (str-1 str-2)
  (concatenate 'string str-1 str-2))

(defun lines-of-file (path)
  (with-open-file (file path
			:direction :input
			:if-does-not-exist :error)
    (loop
       for line = (read-line file nil nil)
       if line collect it into lines else return lines)))

(defmethod file-contents ((file-path pathname))
  (format nil "~{~a~^~%~}" (lines-of-file file-path)))

(defmethod read-tptp ((tptp-file pathname))
  (read-tptp (file-contents tptp-file)))
