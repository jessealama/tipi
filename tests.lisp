
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

(defmacro define-lexer-test ((test-name) tptp-string &rest tokens)
  `(5am:test ,test-name
     (let ((target-lex-result (append (mapcar #'maybe-make-symbol ',tokens)
				      (list nil)))
	   (lexed (lex-tptp-formula ,tptp-string)))
       (5am:is (not (null lexed)))
       (5am:is (starts-with-subseq lexed target-lex-result
				   :test #'same-symbol-or-null))
       (5am:is (starts-with-subseq target-lex-result lexed
				   :test #'same-symbol-or-null)))))

(define-lexer-test (lex-tptp-1)
    "fof(a,axiom,p)."
  "fof" "(" "lower-word" "," "axiom" "," "lower-word" ")" ".")

;;; tests.lisp ends here