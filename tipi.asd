
(in-package :cl-user)

(defpackage :tipi-asd
  (:use :cl :asdf))

(in-package :tipi-asd)

(defsystem :tipi
  :description "A Swiss Army knife of TPTP tools, emphasizing proof analysis."
  :author "Jesse Alama <jesse.alama@gmail.com>"
  :maintainer "Jesse Alama <jesse.alama@gmail.com>"
  :depends-on (:cl-ppcre :com.gigamonkeys.pathnames :named-readtables)
  :serial t
  :components ((:file "packages")
	       (:file "utils")
	       (:file "terms")
	       (:file "formulas")
	       (:file "tptp")))
