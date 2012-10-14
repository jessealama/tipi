
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
  `(test ,test-name
     (let ((target-lex-result (append (mapcar #'maybe-make-symbol ',tokens)
				      (list nil)))
	   (lexed (lex-tptp-formula ,tptp-string)))
       (is (not (null lexed)))
       (is (mismatch lexed target-lex-result :test #'same-symbol-or-null) nil)
       (is (mismatch target-lex-result lexed :test #'same-symbol-or-null) nil))))

(define-lexer-test (lex-tptp-1)
    "fof(a,axiom,p)."
  "fof" "(" "lower-word" "," "axiom" "," "lower-word" ")" ".")

(define-lexer-test (lex-tptp-2)
    "fof(ax,hypothesis,(! [X] : (X = X)))."
  "fof" "(" "lower-word" "," "hypothesis" "," "(" "!" "[" "upper-word" "]" ":" "(" "upper-word" "=" "upper-word" ")" ")" ")" ".")

(test cnf-not-handled
  (signals error (lex-tptp-formula "cnf(a,axiom,(! [X] : (X = X))).")))

;;; tests.lisp ends here
