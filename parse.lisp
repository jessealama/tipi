(in-package :tipi)

;;; The lexer

(define-condition lexer-error (yacc-runtime-error)
  ((character :initarg :character :reader lexer-error-character))
  (:report (lambda (e stream)
             (format stream "Lexing failed~@[: unexpected character ~S~]"
                     (lexer-error-character e)))))

(defun lexer-error (char)
  (error (make-condition 'lexer-error :character char)))

(defun maybe-unread (char stream)
  (when char
    (unread-char char stream)))

(defun read-number (stream)
  (let ((v nil))
    (loop
       (let ((c (read-char stream nil nil)))
         (when (or (null c) (not (digit-char-p c)))
           (maybe-unread c stream)
           (when (null v)
             (lexer-error c))
           (return-from read-number v))
         (setf v (+ (* (or v 0) 10) (- (char-code c) (char-code #\0))))))))

(defun intern-id (string)
  ;; I'd really like to say (intern (string-upcase string) '#.*package*),
  ;; but that breaks Allegro's case hacks.
  (let ((*package* '#.*package*))
    (read-from-string string)))

(defun read-id (stream)
  (let ((v '()))
    (loop
       (let ((c (read-char stream nil nil)))
         (when (or (null c)
                   (not (or (digit-char-p c) (alpha-char-p c) (eql c #\_))))
           (maybe-unread c stream)
           (when (null v)
             (lexer-error c))
           (return-from read-id (intern-id (coerce (nreverse v) 'string))))
         (push c v)))))

(defun lexer (&optional (stream *standard-input*))
  (loop
     (let ((c (read-char stream nil nil)))
       (cond
         ((member c '(#\Space #\Tab)))
         ((member c '(nil #\Newline)) (return-from lexer (values nil nil)))
         ((member c '(#\+ #\- #\* #\/ #\( #\)))
          (let ((symbol (intern (string c) '#.*package*)))
            (return-from lexer (values symbol symbol))))
         ((digit-char-p c)
          (unread-char c stream)
          (return-from lexer (values 'int (read-number stream))))
         ((alpha-char-p c)
          (unread-char c stream)
          (return-from lexer (values 'id (read-id stream))))
         (t
          (lexer-error c))))))

;;; The parser

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun i2p (a b c)
    "Infix to prefix"
    (list b a c))

  (defun k-2-3 (a b c)
    "Second out of three"
    (declare (ignore a c))
    b)
)

(define-parser *tptp-v5.4.0.0-parser*
  (:start-symbol tptp-file)
  (:terminals (atom integer))

  (tptp-file
   ()
   tptp-input)

  (tptp-input
   annotated-formula
   include)

  (annotated-formula
   fof-annotated)

  (fof-annotated
   ("fof" "(" name "," formula-role "," fof-formula ")" "."))

  (name
   atomic-word
   integer)

  (atomic-word
   lower-word
   single-quoted)

  (lower-word
   lower-alpha
   (lower-alpha alpha-numerics))

  (lower-alpha
   "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
   "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")

  (upper-alpha
   "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
   "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z")

  (alpha-numerics
   alpha-numeric
   (alpha-numeric alpha-numerics))

  (alpha-numeric
   lower-alpha
   upper-alpha
   numeric
   "_")

  (numeric
   "0" "1" "2" "3" "4" "5" "6" "7" "8" "9")

  (single-quoted
   (single-quote sq-char sq-chars single-quote))

  (single-quote
   "'")

  (sq-char
   "blah")

  (sq-chars
   ()
   (sq-char sq-chars))

  (formula-role
   "axiom"
   "hypothesis"
   "definition"
   "assumption"
   "lemma"
   "theorem"
   "conjecture"
   "negated_conjecture"
   "plain"
   "fi_domain"
   "fi_functors"
   "fi_predicates"
   "type"
   "unknown")

  (fof-formula
   fof-logic-formula
   fof-sequent)

  (fof-logic-formula
   fof-binary-formula
   fof-unitary-formula)

  (fof-binary-formula
   fof-binary-nonassoc
   fof-binary-assoc)

  (fof-binary-nonassoc
   (fof-unitary-formula binary-connective fof-unitary-formula))

  (fof-binary-assoc
   fof-or-formula
   fof-and-formula)

  (fof-or-formula
   (fof-unitary-formula "|" fof-unitary-formula))

  (fof-and-formula
   (fof-unitary-formula "&" fof-unitary-formula))

  (fof-binary-connective
   "<=>"
   "=>"
   "<="
   "<~>"
   "~|"
   "~&")

  (fof-unitary-formula
   fof-quantified-formula
   fof-unary-formula
   atomic-formula
   ("(" fof-logic-formula ")"))

  (fof-quantified-formula
   (fol-quantifier "[" fof-variable-list "]" ":" fof-unitary-formula))

  (fol-quantifier
   "!"
   "?")

  (fof-variable-list
   variable
   (variable "," fof-variable-list))

  (variable
   upper-word)

  (upper-word
   (upper-alpha alpha-numerics))

  (fof-unary-formula
   (unary-connective fof-unitary-formula)
   fol-infix-unary)

  (unary-connective
   "~")

  (fol-infix-unary
   (term infix-inequality term))

  (term
   function-term
   variable
   conditional-term
   let-term)

  (function-term
   plain-term
   defined-term
   system-term)

  (plain-term
   constant
   (functor "(" arguments ")"))

  (constant
   functor)

  (functor
   atomic-word)

  (arguments
   term
   (term "," arguments))

  (defined-term
      defined-atom
      defined-atomic-term)

  (defined-atom
      number
      distinct-object)

  (number
   integer
   rational
   real)

  (integer
   signed-integer
   unsigned-integer)

  (signed-integer
   (sign unsigned-integer))

  (sign
   "+"
   "-")

  (unsigned-integer
   decimal)

  (decimal
   zero-numeric
   positive-decimal)

  (zero-numeric
   "0")

  (positive-decimal
   (non-zero-numeric numerics))

  (numerics
   ""
   (numeric numerics))

  (non-zero-numeric
   "1" "2" "3" "4" "5" "6" "7" "8" "9")

  (rational
   signed-rational
   unsigned-rational)

  (signed-rational
   (sign unsigned-rational))

  (unsigned-rational
   (decimal slash positive-decimal))

  (slash
   "/")

  (real
   signed-real
   unsigned-real)

  (signed-real
   (sign unsigned-real))

  (unsigned-real
   decimal-fraction
   decimal-exponent)

  (decimal-fraction
   (decimal dot-decimal))

  (dot-decimal
   ("." numeric numerics))

  (decimal-exponent
   (decimal exponent integer)
   (decimal-fraction exponent integer))

  (exponent
   "E"
   "e")

  (distinct-object
   "\"" "distinct" "\"")

  (defined-atomic-term
      defined-plain-term)

  (defined-plain-term
      defined-constant
      (defined-functor "(" arguments ")"))

  (defined-constant
      defined-functor
      defined-type)

  (defined-type
      "$oType"
      "$o"
    "$iType"
    "$tType"
    "$real"
    "$rat"
    "$int")

  (system-term
   system-constant
   (system-functor "(" arguments ")"))

  (system-constant
   system-functor)

  (system-functor
   atomic-system-word)

  (atomic-system-word
   dollar-dollar-word)

  (dollar-dollar-word
   ("$$" lower-word))

  (conditional-term
   "$ite_t(" tff-logic-formula "," term "," term ")")

  (let-term
   ("$let_ft(" tff-let-formula-defn "," term ")")
   ("$let_ft(" tff-let-term-defn "," term ")"))

  (infix-inequality
   "!=")

  (defined-functor
      "$uminus"
      "$sum"
    "$difference"
    "$product"
    "$quotient"
    "$quotient_e"
    "$quotient_t"
    "$quotient_f"
    "$remainder_e"
    "$remainder_t"
    "$remainder_f"
    "$floor"
    "$ceiling"
    "$truncate"
    "$round"
    "$to_int"
    "$to_rat"
    "$to_real")

)


;;; The evaluator

(define-condition evaluator-error (yacc-runtime-error)
  ((expression :initarg :expression :reader evaluator-error-expression))
  (:report (lambda (e stream)
             (format stream "Couldn't evaluate expression ~S"
                     (evaluator-error-expression e)))))

(defun evaluate (e)
  (labels ((fail () (error (make-condition 'evaluator-error :expression e))))
    (cond
      ((numberp e) e)
      ((eql 'pi e) (int-or-float pi))
      ((eql 'e e) (int-or-float (exp 1)))
      ((atom e) (fail))
      ((= 2 (length e))
       (unless (eql '- (car e)) (fail))
       (- (evaluate (cadr e))))
      ((= 3 (length e))
       (funcall (binop (car e)) (evaluate (cadr e)) (evaluate (caddr e))))
      (t (fail)))))

;;; The toplevel loop

(defun calculator ()
  (format t "Type an infix expression to evaluate it, an empty line to quit.~%")
  (loop
     (with-simple-restart (abort "Return to calculator toplevel.")
       (format t "? ")
       (let ((e (parse-with-lexer #'lexer *tptp-parser*)))
         (when (null e)
           (return-from calculator))
         (format t " => OK~%")))))
