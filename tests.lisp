
(in-package :tipi)

(defparameter *tptp-formula-1* "fof(a,axiom,p).")

(let ((symbol-table (make-hash-table :test #'equal)))
  (defun maybe-make-symbol (name)
    (multiple-value-bind (old-symbol present?)
	(gethash name symbol-table)
      (if present?
	  old-symbol
	  (let ((new-symbol (make-symbol name)))
	    (setf (gethash name symbol-table) new-symbol)
	    new-symbol)))))

(defun same-symbol-or-null (x y)
  (cond ((null x)
	 (null y))
	((symbolp x)
	 (and (symbolp y)
	      (string= (symbol-name x) (symbol-name y))))
	(t
	 nil)))

(5am:test lex-tptp
  (let ((target-first-lex-result (list (maybe-make-symbol "fof")
				       (maybe-make-symbol "(")
				       (maybe-make-symbol "lower-word")
				       (maybe-make-symbol ",")
				       (maybe-make-symbol "axiom")
				       (maybe-make-symbol ",")
				       (maybe-make-symbol "lower-word")
				       (maybe-make-symbol ")")
				       (maybe-make-symbol ".")
				       nil))
	(lexed (lex-tptp-formula *tptp-formula-1*)))
    (5am:is (not (null lexed)))
    (5am:is (starts-with-subseq lexed target-first-lex-result
				:test #'same-symbol-or-null))
    (5am:is (starts-with-subseq target-first-lex-result lexed
				:test #'same-symbol-or-null))))

;;; tests.lisp ends here
