
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

(defgeneric same-symbol-or-null (x y))

(defmethod same-symbol-or-null ((x t) (y t))
  nil)

(defmethod same-symbol-or-null ((x symbol) (y symbol))
  (string= (symbol-name x) (symbol-name y)))

(defmethod same-symbol-or-null ((x null) (y null))
  t)

(defmacro define-lexer-test ((test-name) tptp-string &rest tokens)
  `(test ,test-name
     (let ((target-lex-result (append (mapcar #'maybe-make-symbol ',tokens)
				      (list nil)))
	   (lexed (lex-tptp-formula ,tptp-string)))
       (is (null (mismatch lexed target-lex-result :test #'same-symbol-or-null)))
       (is (null (mismatch target-lex-result lexed :test #'same-symbol-or-null))))))

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
