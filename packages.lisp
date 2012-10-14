
(in-package :cl-user)

(defpackage :tipi
  (:use :cl)
  (:use :yacc)
  (:import-from :trivial-timeout
		#:with-timeout
		#:timeout-error)
  (:import-from :named-readtables
		#:find-readtable
		#:defreadtable
		#:in-readtable)
  (:import-from :cl-fad
		#:file-exists-p
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
