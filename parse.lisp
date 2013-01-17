
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

(defun read-word (stream)
  (let ((v '()))
    (loop
       (let ((c (read-char stream nil nil)))
         (when (or (null c)
                   (not (or (digit-char-p c)
			    (alpha-char-p c)
			    (eql c #\_))))
           (maybe-unread c stream)
           (when (null v)
             (lexer-error c))
           (return-from read-word (coerce (nreverse v) 'string)))
         (push c v)))))

(defun read-quoted-atom (stream)
  (declare (ignore stream))
  (error "We don't handle quoted atoms yet, sorry.  Please complain loudly."))

(defparameter *tptp-keywords*
  (list

   ;; annotated formula keywords
   "fof"
   "cnf"

   ;; formula roles
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
   "unknown"

	))

(defparameter *whitespace-characters*
  '(#\Space #\Tab #\Newline))

(let (expecting-keyword num-left-parens-seen num-commas-seen expecting-formula)

  (defun initialize-lexer ()
    (setf expecting-keyword t
	  expecting-formula nil
	  num-left-parens-seen 0
	  num-commas-seen 0)
    t)

  (defun lexer-report-state ()
    (format t "Num left parens seen: ~d~%Num commas seen: ~d~%Expecting TPTP keyword: ~a~%" num-left-parens-seen num-commas-seen (if expecting-keyword "yes" "no")))

  (defun lexer (&optional (stream *standard-input*))
    (loop
       for c = (read-char stream nil nil)
       do
	 (cond

	   ((null c)
	    (return-from lexer (values nil nil)))

	   ((char= c #\%)
	    (read-line stream)) ;; consume comment lines

	   ((member c *whitespace-characters*)) ;; consume whitespace

	   ((char= c #\')
	    (unread-char c stream)
	    (let ((quoted (read-quoted-atom stream)))
	      (return-from lexer (values (intern "quoted") quoted))))

	   ((member c '(#\( #\) #\. #\[ #\] #\: #\! #\? #\, #\< #\= #\&))
	    ;; (break "Got a symbol: ~a" c)
	    (when (char= c #\()
	      (incf num-left-parens-seen))
	    (when (char= c #\,)
	      (incf num-commas-seen))
	    (setf expecting-keyword
		  (or (zerop num-left-parens-seen)
		      (and (= num-left-parens-seen 1)
			   (= num-commas-seen 1)))
		  expecting-formula
		  (= num-commas-seen 2))
	    (when (char= c #\.)
	      (initialize-lexer))

	    (when (char= c #\=)
	      (let ((d (read-char stream nil nil)))
		(if d
		    (if (char= d #\>)
			(return-from lexer (values (intern "=>") "=>"))
			(progn
			    (unread-char d stream)
			    (return-from lexer (values (intern "=") "="))))
		    (lexer-error d))))

	    (return-from lexer (values (intern (string c)) (string c))))

	   ;; try to read an atom
	   ((alpha-char-p c)
	    (unread-char c stream)
	    (let ((next-word (read-word stream)))
	      ;; (break "next-word = ~a" next-word)
	      (if expecting-keyword
		  (if (member next-word *tptp-keywords* :test #'string=)
		      (return-from lexer (values (intern next-word) next-word))
		      (error "We are expecting a TPTP keyword, but~%~%  ~a~%~%isn't a known keyword.  The known keywords are:~%~%~{  ~a~%~}~%" next-word *tptp-keywords*))
		  (if (lower-case-p c)
		      (return-from lexer (values (intern "lower-word") next-word))
		      (return-from lexer (values (intern "upper-word") next-word))))))

	   (t
	    (lexer-error c))))))

;;; The parser

(define-parser *tptp-v5.4.0.0-parser*
  (:start-symbol tptp-file)
  (:terminals (|fof|
	       |cnf|
	       |(|
	       |)|
	       |.|
	       |,|
	       |[|
	       |]|
	       |=|
	       |%|
	       |lower-word|
	       |upper-word|
	       |alpha-numeric|
	       |numeric|
	       ;; |a| |b| |c| |d| |e| |f| |g| |h| |i| |j| |k| |l| |m|
	       ;; |n| |o| |p| |q| |r| |s| |t| |u| |v| |w| |x| |y| |z|
	       ;; |A| |B| |C| |D| |E| |F| |G| |H| |I| |J| |K| |L| |M|
	       ;; |N| |O| |P| |Q| |R| |S| |T| |U| |V| |W| |X| |Y| |Z|
	       ;; |0| |1| |2| |3| |4| |5| |6| |7| |8| |9|
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
	       |!=|
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
   fof-annotated
   cnf-annotated)

  (fof-annotated
   (|fof| |(| name |,| formula-role |,| fof-formula |)| |.|))

  (name
   atomic-word
   integer)

  (atomic-word
   |lower-word|)

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

  (atomic-formula
   plain-atomic-formula
   defined-atomic-formula
   ;; system-atomic-formula
   )

  (plain-atomic-formula
   plain-term)

  (defined-atomic-formula
      defined-plain-formula
      defined-infix-formula)

  (defined-infix-formula
      (term defined-infix-pred term))

  (defined-infix-pred
      infix-equality)

  (infix-equality
   |=|)

  (defined-plain-formula
      defined-plain-term)

  (fol-quantifier
   |!|
   |?|)

  (fof-variable-list
   variable
   (variable |,| fof-variable-list))

  (variable
   |upper-word|)

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
      ;; number
      ;; distinct-object
      )

  (defined-atomic-term
      defined-plain-term)

  (defined-plain-term
      defined-constant
      (defined-functor |(| arguments |)|))

  (defined-constant
      defined-functor
      defined-type)

  (infix-inequality
   |!=|)

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
  (initialize-lexer)
  (loop
     (with-simple-restart (abort "Return to calculator toplevel.")
       (format t "? ")
       (let ((e (parse-with-lexer #'lexer *tptp-v5.4.0.0-parser*)))
         (when (null e)
           (return-from calculator))
         (format t " => ~a~%" e)))))

(defun parse-tptp-file (tptp-path)
  (with-open-file (tptp-stream tptp-path
			       :direction :input
			       :if-does-not-exist :error)
    (let ((*standard-input* tptp-stream))
      (initialize-lexer)
      (parse-with-lexer #'lexer
			*tptp-v5.4.0.0-parser*))))

(defun lex-tptp-formula (string)
  (with-input-from-string (stream string)
    (loop
       initially (initialize-lexer)
       for token = (lexer stream)
       collect token into tokens
       unless token return tokens)))

;;; parse.lisp ends here
