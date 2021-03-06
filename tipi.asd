
(in-package :cl-user)

(defpackage :tipi-asd
  (:use :cl :asdf))

(in-package :tipi-asd)

(defsystem :tipi
  :description "A Swiss Army knife of TPTP tools, emphasizing proof analysis."
  :author "Jesse Alama <jesse.alama@gmail.com>"
  :maintainer "Jesse Alama <jesse.alama@gmail.com>"
  :depends-on (:cl-ppcre
	       :cl-fad
	       :named-readtables
	       :alexandria
	       :trivial-timeout)
  :serial t
  :components ((:file "packages")
	       (:file "utils")
	       (:file "xslt")
	       (:file "run")
	       (:file "terms")
	       (:file "formulas")
	       (:file "szs")
	       (:file "result")
	       (:file "tptp")
	       (:file "solve")
	       (:file "model")
	       (:file "consequence")
	       (:file "needed")
	       (:file "minimal")
	       (:file "independent")))
