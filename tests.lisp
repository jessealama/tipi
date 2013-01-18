
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

(defun mismatch-symbols (list-1 list-2)
  "MISMATCH on LIST-1 and LIST-2, assuming that they are both lists of
  symbols and the symbols are to be compared by their name."
  (mismatch list-1 list-2 :test #'string= :key #'symbol-name))

(defmacro define-lexer-test ((test-name) tptp-string &rest tokens)
  `(test ,test-name
     (let ((target-lex-result (mapcar #'maybe-make-symbol ',tokens))
	   (lexed (lex-tptp-formula ,tptp-string)))
       (is (null (first (last lexed))))
       (let ((lexed-butlast (butlast lexed)))
	 (is (null (mismatch-symbols lexed-butlast target-lex-result)))
	 (is (null (mismatch-symbols target-lex-result lexed-butlast)))))))

(define-lexer-test (lex-tptp-1)
    "fof(a,axiom,p)."
  "fof" "(" "lower-word" "," "axiom" "," "lower-word" ")" ".")

(define-lexer-test (lex-tptp-2)
    "fof(ax,hypothesis,(! [X] : (X = X)))."
  "fof" "(" "lower-word" "," "hypothesis" "," "(" "!" "[" "upper-word" "]" ":" "(" "upper-word" "=" "upper-word" ")" ")" ")" ".")

(define-lexer-test (lex-tptp-3)
    "fof(def_o,axiom,(! [X] : (object(X) => (? [D2] : (object(D2))))))."
  "fof" "(" "lower-word" "," "axiom" "," "(" "!" "[" "upper-word" "]" ":" "(" "lower-word" "(" "upper-word" ")" "=>" "(" "?" "[" "upper-word" "]" ":" "(" "lower-word" "(" "upper-word" ")" ")" ")" ")" ")" ")" ".")

(define-lexer-test (lex-test-4)
    "cnf(a,axiom,(! [X] : (X = X)))."
  "cnf" "(" "lower-word" "," "axiom" "," "(" "!" "[" "upper-word" "]" ":" "(" "upper-word" "=" "upper-word" ")" ")" ")" ".")

;;; tests.lisp ends here
