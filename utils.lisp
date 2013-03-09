
(in-package :tipi)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric symbolify-here (thing))

(defmethod symbolify-here ((thing symbol))
  (intern (symbol-name thing) (find-package :tipi)))

(defmethod symbolify-here ((thing string))
  (intern thing (find-package :tipi)))

(defun empty-string? (thing)
  (when (stringp thing)
    (string= thing "")))

(defun equal-as-strings? (thing-1 thing-2)
  (string= (format nil "~a" thing-1)
	   (format nil "~a" thing-2)))

(defun stream-lines (stream)
  (when stream
    (let (lines)
      (symbol-macrolet
	  (($line (read-line stream nil nil)))
	(do ((line $line $line))
	    ((null line))
	  (push line lines)))
      (reverse lines))))

(defun stream-contents (stream)
  (format nil "~{~a~%~}" (stream-lines stream)))

(defun temporary-file (&key (base "") (extension "") (tmp-dir "/tmp"))
  (if (or (stringp tmp-dir)
	  (pathnamep tmp-dir))
      (if (stringp extension)
	  (if (scan "^\.?[a-zA-Z0-9]*$" extension)
	      (if (stringp base)
		  (if (scan "^[A-Za-z]*$" base)
		      (register-groups-bind (real-ext)
			  ("^\.?([a-zA-Z0-9]*)$" extension)
			(let ((real-tmp-dir (pathname-as-directory tmp-dir)))
			  (if (directory-pathname-p real-tmp-dir)
			      (loop
				 with real-tmp-dir-name = (namestring real-tmp-dir)
				 for i from 1 upto 100000
				 for tmp-path = (if (string= real-ext "")
						    (format nil "~a/~a~d" real-tmp-dir-name base i)
						    (format nil "~a/~a~d.~a" real-tmp-dir-name base i real-ext))
				 do
				   (unless (probe-file tmp-path)
				     (return (pathname tmp-path)))
				 finally
				   (if (string= base "")
				       (error "We have run out of temporary file names in ~a!" tmp-dir)
				       (error "We have run out of temporary file names in ~a! with the base name ~a" tmp-dir base)))
			      (error "We cannot understand '~a' as a directory" tmp-dir))))
		      (error "BASE must consist of alphanumeric characters only; '~a' is not a suitable argument" base))
		  (error "BASE must be a string; '~a' is not a suitable argument" base))
	      (error "EXTENSION must be a string consisting of alphanumeric characters, possibly beginning with a period '.'; '~a' is not a suitable argument" extension))
	  (error "EXTENSION must be a string (possibly the empty string); '~a' is not a suitable value" extension))
      (error "TMP-DIR must be either a string or a pathname; '~a' is not a suitable argument" tmp-dir)))

(defun file-contents (path)
  (with-open-file (file path
			:direction :input
			:if-does-not-exist :error)
    (stream-contents file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Input and output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This could be done iteratively rather than recursively, but I'm not
;; sure it makes much of a difference.  I don't expect this function
;; to get called very much anyway.
(defun comma-separated-list (lst)
  (format nil "~{~a~^, ~}" lst))

(defmacro with-readtable (readtable &body body)
  `(let ((*readtable* ,readtable))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hash tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hash-table-keys (table)
  (loop for k being the hash-keys in table collect k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun split-lines (text)
  (split #\Newline text))

(defun stringify (x)
  (format nil "~a" x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun atoms-in-list (thing list &key test)
  "All atoms under THING that belong to LIST.  TEST is used to determine membership.  It is permitted that THING is an atom."
  (if (null list)
      nil
      (let ((flattened (flatten thing)))
	(let ((flattened-no-dups (remove-duplicates flattened :test test)))
	  (loop
	     for atom in flattened-no-dups
	     when (member atom list :test test) collect atom into atoms
	     finally (return atoms))))))

(defun some-subset (set set-of-sets &key test key)
  (some #'(lambda (other-set)
	    (subsetp other-set set :test test :key key))
	set-of-sets))

(defun map-all-combinations (function list)
  (loop
     for n from 0 upto (length list)
     do
       (map-combinations function list :length n)
     finally
       (return t)))

(defun extract-common-elements (test lists)
  (let ((all-elements-table (make-hash-table :test #'equal))
	(common-elements nil))
    (dolist (list lists)
      (dolist (elt list)
	(setf (gethash elt all-elements-table) 0)))
    (dolist (elt (hash-table-keys all-elements-table))
      (when (every #'(lambda (list) (member elt list :test test)) lists)
	(push elt common-elements)))
    (cons common-elements
	  (mapcar #'(lambda (list)
		      (remove-if #'(lambda (elt)
				     (member elt common-elements :test test))
				 list))
		  lists))))

(defun native-namestring (path)
  #+ccl
  (ccl:native-translated-namestring path)
  #+sbcl
  (sb-ext:native-namestring path)
  #-(or ccl sbcl)
  (namestring path))

(defun current-directory ()
  #+sbcl (sb-posix:getcwd)
  #+ccl (ccl:current-directory)
  #-(or sbcl ccl) (error "We support only SBCL and CCL.")
  )

(defun chdir (dir)
  #+sbcl
  (sb-posix:chdir dir)
  #+ccl
  (ccl:cwd dir)
  #-(or sbcl ccl)
  (error "We support only SBCL and CCL.")
  )

(defmacro with-current-directory ((new-cwd) &body body)
  (let ((cwd (gensym)))
    `(let ((,cwd (current-directory)))
       (chdir ,new-cwd)
       (unwind-protect
	    (progn ,@body)
	 (chdir ,cwd)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun first-n-numbers (n)
  (loop
     :for i :from 1 :upto n
     :collect i :into numbers
     :finally (return numbers)))
