
(in-package :cl-user)

(defpackage :tipi
  (:use :cl)
  (:use :yacc)
  (:import-from :cl-who
		#:with-html-output-to-string)
  (:use :parallel)
  (:use :hunchentoot)
  (:use :hunchentoot-utils)
  (:use :cl-who)
  (:import-from :named-readtables
		#:in-readtable)
  (:import-from :cl-fad
		#:file-exists-p
		#:directory-exists-p
		#:directory-pathname-p
		#:pathname-as-directory)
  (:import-from :alexandria
		#:length=
		#:define-constant
		#:map-combinations
		#:write-string-into-file
		#:hash-table-values
		#:starts-with-subseq
		#:flatten)
  (:import-from :cl-ppcre
		#:scan
		#:split
		#:register-groups-bind)
  (:import-from :5am
	       #:test
	       #:signals
	       #:is)
  (:export
   #:MINIMIZE))
