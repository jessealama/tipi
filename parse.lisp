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

(defun intern-id (string)
  ;; I'd really like to say (intern (string-upcase string) '#.*package*),
  ;; but that breaks Allegro's case hacks.
  (let ((*package* '#.*package*))
    (read-from-string string)))

(defun read-word (stream)
  (let ((v '()))
    (loop
       (let ((c (read-char stream nil nil)))
         (when (or (null c)
                   (not (or (digit-char-p c) (alpha-char-p c) (eql c #\_))))
           (maybe-unread c stream)
           (when (null v)
             (lexer-error c))
           (return-from read-word (make-symbol (coerce (nreverse v) 'string))))
         (push c v)))))

(defun lexer (&optional (stream *standard-input*))
  (loop
     (let ((c (read-char stream nil nil)))
       (cond
	 ((member c '(#\Space #\Tab)))
	 ((member c '(nil #\Newline)) (return-from lexer (values nil nil)))

	 ((member c '(#\( #\) #\. #\' #\[ #\] #\: #\! #\? #\, #\=))
	  ;; (break "Got a symbol: ~a" c)
          (let ((symbol (intern (string c) '#.*package*)))
            (return-from lexer (values symbol symbol))))

	 ;; try to read an atom
	 ((alpha-char-p c)
	  (unread-char c stream)
	  (let ((next-word (read-word stream)))
	    ;; (break "next-word = ~a" next-word)
	    (return-from lexer (values next-word (symbol-name next-word)))))

	 (t
	  (lexer-error c))))))

;;; The parser

(define-parser *tptp-v5.4.0.0-parser*
  (:start-symbol tptp-file)
  (:terminals (|fof|
	       |(|
	       |)|
	       |.|
	       |,|
	       |[| |]|
	       |a| |b| |c| |d| |e| |f| |g| |h| |i| |j| |k| |l| |m|
	       |n| |o| |p| |q| |r| |s| |t| |u| |v| |w| |x| |y| |z|
	       |A| |B| |C| |D| |E| |F| |G| |H| |I| |J| |K| |L| |M|
	       |N| |O| |P| |Q| |R| |S| |T| |U| |V| |W| |X| |Y| |Z|
	       |0| |1| |2| |3| |4| |5| |6| |7| |8| |9|
	       |_|
	       |'|
	       |axiom|
	       |hypothesis|
	       |definition|
	       |assumption|
	       |lemma|
	       |theorem|
	       |conjecture|
	       |negated_conjecture|
	       |plain|
	       |fi_domain|
	       |fi_functors|
	       |fi_predicates|
	       |type|
	       |unknown|
	       |<=>|
	       |=>|
	       |<=|
	       |<~>|
	       ;; |~|
	       |~&|
	       |:|
	       |!|
	       |?|
	       |~|
	       |+|
	       |-|
	       |/|
	       |E|
	       |e|
	       |!=|
	       |$oType|
	       |$o|
	       |$iType|
	       |$tType|
	       |$real|
	       |$rat|
	       |$int|

	       |$uminus|
	       |$sum|
	       |$difference|
	       |$product|
	       |$quotient|
	       |$quotient_e|
	       |$quotient_t|
	       |$quotient_f|
	       |$remainder_e|
	       |$remainder_t|
	       |$remainder_f|
	       |$floor|
	       |$ceiling|
	       |$truncate|
	       |$round|
	       |$to_int|
	       |$to_rat|
	       |$to_real|
	       |$$|
	       |&|
	       |\||
	       ))

  (tptp-file
   tptp-inputs)

  (tptp-inputs
   ()
   (tptp-input tptp-inputs))

  (tptp-input
   annotated-formula
   include)

  (annotated-formula
   fof-annotated)

  (fof-annotated
   (|fof| |(| name |,| formula-role |,| fof-formula |)| |.|))

  (name
   atomic-word
   integer)

  (atomic-word
   lower-word)

  (lower-word
   lower-alpha
   (lower-alpha alpha-numerics))

  (lower-alpha
   |a| |b| |c| |d| |e| |f| |g| |h| |i| |j| |k| |l| |m|
   |n| |o| |p| |q| |r| |s| |t| |u| |v| |w| |x| |y| |z|)

  (upper-alpha
   |A| |B| |C| |D| |E| |F| |G| |H| |I| |J| |K| |L| |M|
   |N| |O| |P| |Q| |R| |S| |T| |U| |V| |W| |X| |Y| |Z|)

  (alpha-numerics
   alpha-numeric
   (alpha-numeric alpha-numerics))

  (alpha-numeric
   lower-alpha
   upper-alpha
   numeric
   |_|)

  (numeric
   |0| |1| |2| |3| |4| |5| |6| |7| |8| |9|)

  (formula-role
   |axiom|
   |hypothesis|
   |definition|
   |assumption|
   |lemma|
   |theorem|
   |conjecture|
   |negated_conjecture|
   |plain|
   |fi_domain|
   |fi_functors|
   |fi_predicates|
   |type|
   |unknown|)

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
   (fof-unitary-formula |\|| fof-unitary-formula))

  (fof-and-formula
   (fof-unitary-formula |&| fof-unitary-formula))

  (fof-binary-connective
   |<=>|
   |=>|
   |<=|
   |<~>|
   ;; |~\||
   |~&|)

  (fof-unitary-formula
   fof-quantified-formula
   fof-unary-formula
   atomic-formula
   (|(| fof-logic-formula |)|))

  (fof-quantified-formula
   (fol-quantifier |[| fof-variable-list |]| |:| fof-unitary-formula))

  (fol-quantifier
   |!|
   |?|)

  (fof-variable-list
   variable
   (variable |,| fof-variable-list))

  (variable
   upper-word)

  (upper-word
   (upper-alpha alpha-numerics))

  (fof-unary-formula
   (unary-connective fof-unitary-formula)
   fol-infix-unary)

  (unary-connective
   |~|)

  (fol-infix-unary
   (term infix-inequality term))

  (term
   function-term
   variable
   ;; conditional-term
   ;; let-term
   )

  (function-term
   plain-term
   defined-term
   system-term)

  (plain-term
   constant
   (functor |(| arguments |)|))

  (constant
   functor)

  (functor
   atomic-word)

  (arguments
   term
   (term |,| arguments))

  (defined-term
      defined-atom
      defined-atomic-term)

  (defined-atom
      number
      ;; distinct-object
      )

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
   |+|
   |-|)

  (unsigned-integer
   decimal)

  (decimal
   zero-numeric
   positive-decimal)

  (zero-numeric
   |0|)

  (positive-decimal
   (non-zero-numeric numerics))

  (numerics
   ()
   (numeric numerics))

  (non-zero-numeric
   |1| |2| |3| |4| |5| |6| |7| |8| |9|)

  (rational
   signed-rational
   unsigned-rational)

  (signed-rational
   (sign unsigned-rational))

  (unsigned-rational
   (decimal slash positive-decimal))

  (slash
   |/|)

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
   (|.| numeric numerics))

  (decimal-exponent
   (decimal exponent integer)
   (decimal-fraction exponent integer))

  (exponent
   |E|
   |e|)

  ;; (distinct-object
  ;;  "\"" "distinct" "\"")

  (defined-atomic-term
      defined-plain-term)

  (defined-plain-term
      defined-constant
      (defined-functor |(| arguments |)|))

  (defined-constant
      defined-functor
      defined-type)

  (defined-type
      |$oType|
      |$o|
    |$iType|
    |$tType|
    |$real|
    |$rat|
    |$int|)

  (system-term
   system-constant
   (system-functor |(| arguments |)|))

  (system-constant
   system-functor)

  (system-functor
   atomic-system-word)

  (atomic-system-word
   dollar-dollar-word)

  (dollar-dollar-word
   (|$$| lower-word))

  ;; (conditional-term
  ;;  "$ite_t(" tff-logic-formula "," term "," term ")")

  ;; (let-term
  ;;  ("$let_ft(" tff-let-formula-defn "," term ")")
  ;;  ("$let_ft(" tff-let-term-defn "," term ")"))

  (infix-inequality
   |!=|)

  (defined-functor
      |$uminus|
      |$sum|
    |$difference|
    |$product|
    |$quotient|
    |$quotient_e|
    |$quotient_t|
    |$quotient_f|
    |$remainder_e|
    |$remainder_t|
    |$remainder_f|
    |$floor|
    |$ceiling|
    |$truncate|
    |$round|
    |$to_int|
    |$to_rat|
    |$to_real|)

)


;;; The evaluator

(define-condition evaluator-error (yacc-runtime-error)
  ((expression :initarg :expression :reader evaluator-error-expression))
  (:report (lambda (e stream)
             (format stream "Couldn't evaluate expression ~S"
                     (evaluator-error-expression e)))))

;;; The toplevel loop

(defun calculator ()
  (format t "Type an infix expression to evaluate it, an empty line to quit.~%")
  (loop
     (with-simple-restart (abort "Return to calculator toplevel.")
       (format t "? ")
       (let ((e (parse-with-lexer #'lexer *tptp-v5.4.0.0-parser*)))
         (when (null e)
           (return-from calculator))
         (format t " => ~a~%" e)))))
