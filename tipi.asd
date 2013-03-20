(asdf:defsystem :tipi
  :description "A Swiss Army knife of TPTP tools, emphasizing proof analysis."
  :author "Jesse Alama <jesse.alama@gmail.com>"
  :maintainer "Jesse Alama <jesse.alama@gmail.com>"
  :depends-on (:cl-ppcre
	       :cl-fad
	       :yacc
	       :named-readtables
	       :hunchentoot-utils
	       :drakma
	       :hunchentoot
	       :cl-who
	       :alexandria
	       :fiveam
	       :cl-parallel)
  :serial t
  :components ((:file "packages")
	       (:file "utils")
	       (:file "queue")
	       (:file "xslt")
	       (:file "run")
	       (:file "expressions")
	       (:file "signature")
	       (:file "szs")
	       (:file "result")
	       (:file "tptp")
	       (:file "tstp")
	       (:file "solve")
	       (:file "model")
	       (:file "consequence")
	       (:file "needed")
	       (:file "minimal")
	       (:file "independent")
	       (:file "parse")
	       (:file "site")
	       (:file "tests")))
