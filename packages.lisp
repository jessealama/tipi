
(in-package :cl-user)

(defpackage :tipi
  (:use :cl)
  (:import-from :trivial-timeout
		#:with-timeout
		#:timeout-error)
  (:import-from :named-readtables
		#:find-readtable)
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
		#:flatten)
  (:import-from :cl-ppcre
		#:scan
		#:split
		#:register-groups-bind)
  (:export
   #:MINIMIZE))
