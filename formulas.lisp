;;; formulas.lisp A representation for propositional and first-order formulas

(in-package :tipi)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass formula ()
  nil)

(defun formula? (thing)
  (typep thing 'formula))

(defclass atomic-formula (formula)
  ((predicate :initarg :predicate
	      :accessor predicate)
   (args :initarg :args
	 :accessor arguments
	 :type list)))

(defun atomic-formula? (thing)
  (typep thing 'atomic-formula))

(defclass unary-connective-formula (composite-formula)
  ((argument :initarg :argument
	     :accessor argument)))

(defclass negation (unary-connective-formula)
  nil)

(defclass binary-connective-formula (composite-formula)
  ((lhs :initarg :lhs
	:accessor lhs
	:type formula)
   (rhs :initarg :rhs
	:accessor rhs
	:type formula)))

(defclass binary-conjunction (binary-connective-formula)
  nil)

(defclass binary-disjunction (binary-connective-formula)
  nil)

(defclass implication (binary-connective-formula)
  nil)

(defclass equivalence (binary-connective-formula)
  nil)

;; quantifiers

(defclass generalization (composite-formula)
  ((bound-variable :initarg :bound-variable
		   :accessor bound-variable
		   :type variable-term)
   (matrix :initarg :matrix
	   :accessor matrix
	   :type formula)))

(defclass universal-generalization (generalization)
  nil)

(defclass existential-generalization (generalization)
  nil)

(defmethod print-object ((atom atomic-formula) stream)
  (let ((pred (predicate atom))
	(args (arguments atom)))
    (if (null args)
	(format stream "~A" pred)
	(progn
	  (format stream "~A" pred)
	  (format stream "(")
	  (format stream "~A" (first args))
	  (loop for arg in (cdr args)
	     do
	       (format stream ",~A" arg))
	  (format stream ")")))))

(defgeneric render-plainly (statement))

(defgeneric render-fancily (statement))

(defmethod render-plainly ((statement term))
  (let ((func-sym (function-symbol statement))
	(args (arguments statement)))
    (if (null args)
	(format nil "~A" func-sym)
	(if (null (cdr args))
	    (format nil "~A(~A)"
		    func-sym
		    (render-plainly (car args)))
	    (concatenate 'string
			(format nil "~A" func-sym)
			"("
			(render-plainly (car args))
			(apply #'concatenate 'string
			       (mapcar #'(lambda (arg)
					   (format nil ",~A" (render-plainly arg)))
				       (cdr args)))
			")")))))

(defmethod render-fancily ((statement term))
  (render-plainly statement))

(defmethod render-plainly :around ((formula unary-connective-formula))
  (let ((body (call-next-method)))
    (concatenate 'string body (render-plainly (argument formula)))))

(defmethod render-fancily :around ((formula unary-connective-formula))
  (format nil "~a~a" (call-next-method) (render-fancily (argument formula))))

(defmethod render-plainly ((neg negation))
  "~")

(defmethod render-fancily ((neg negation))
  "¬")

(defmethod render-plainly :around ((formula binary-connective-formula))
  (concatenate 'string
	       "("
	       (render-plainly (lhs formula))
	       " "
	       (call-next-method)
	       " "
	       (render-plainly (rhs formula))
	       ")"))

(defmethod render-fancily :around ((formula binary-connective-formula))
  (format nil "(~a ~a ~a)"
	  (render-fancily (lhs formula))
	  (call-next-method)
	  (render-fancily (rhs formula))))

(defmethod render-plainly :around ((gen generalization))
  (concatenate 'string
	       (call-next-method)
	       (render-plainly (bound-variable gen))
	       "["
	       (render-plainly (matrix gen))
	       "]"))

(defmethod render-fancily :around ((gen generalization))
  (format nil "~a~a[~a]"
	  (call-next-method)
	  (render-fancily (bound-variable gen))
	  (render-fancily (matrix gen))))

(defmethod render-plainly ((formula binary-conjunction))
  "&")

(defmethod render-fancily ((formula binary-conjunction))
  "∧")

(defmethod render-plainly ((formula binary-disjunction))
  "v")

(defmethod render-fancily ((formula binary-disjunction))
  "∨")

(defmethod render-plainly ((formula implication))
  "-->")

(defmethod render-fancily ((formula implication))
  "→")

(defmethod render-plainly ((formula equivalence))
  "<-->")

(defmethod render-fancily ((formula equivalence))
  "↔")

(defmethod render-plainly ((formula universal-generalization))
  "forall")

(defmethod render-fancily ((formula universal-generalization))
  "∀")

(defmethod render-plainly ((formula existential-generalization))
  "exists")

(defmethod render-fancily ((formula existential-generalization))
  "∃")

(defmethod render-plainly ((formula atomic-formula))
  (let ((pred (predicate formula))
	(args (arguments formula)))
    (if (null args)
	(format nil "~(~a~)" pred)
	(if (null (cdr args))
	    (format nil "~(~a~)(~a)"
		    pred
		    (render-plainly (car args)))
	    (concatenate 'string
			 (format nil "~A" pred)
			 "("
			 (render-plainly (car args))
			 (apply #'concatenate
				'string
				(mapcar #'(lambda (arg)
					    (format nil ",~A" (render-plainly arg)))
					(cdr args)))
			 ")")))))

(defmethod render-fancily ((formula atomic-formula))
  (format nil "<i>~a</i>" (render-plainly formula)))

(defgeneric make-atomic-formula (predicate &rest arguments))

(let ((atomic-formula-store (make-hash-table)))
  (defmethod make-atomic-formula ((predicate symbol) &rest arguments)
    (or (gethash predicate atomic-formula-store)
	(setf (gethash predicate atomic-formula-store)
	      (make-instance 'atomic-formula
			     :predicate predicate
			     :args arguments)))))

(defparameter contradiction (make-atomic-formula 'bottom))

(defparameter top (make-atomic-formula 'top))

(defun make-equation (lhs rhs)
  (make-atomic-formula '= lhs rhs))

(defclass composite-formula (formula)
  nil)

(defun composite-formula? (formula)
  "Determine whether a formula is non-atomic.

Note that, unlike other predicates such as BINARY-DISJUNCTION? and
UNIVERSAL-GENERALIZATION?, this predicate does not merely test whether
the direct class of its argument is COMPOSITE-FORMULA.  The class
COMPOSITE-FORMULA is defined only to provide a common superclass for
further subclasses, such as BINARY-DISJUNCTION and
UNIVERSAL-GENERALIZATION, that is intended to be disjoint from the
class ATOMIC-FORMULA.  This function expresses that disjointedness."
  (and (formula? formula)
       (not (atomic-formula? formula))))

(defun binary-connective-formula? (thing)
  (typep thing 'binary-connective-formula))

(defmethod print-object :around ((formula binary-connective-formula) stream)
  (format stream "(~A " (lhs formula))
  (call-next-method)
  (format stream " ~A)" (rhs formula)))

(defmethod print-object :around ((formula unary-connective-formula) stream)
  (call-next-method)
  (format stream "~A" (argument formula)))

(defgeneric unnegate (formula))

(defmethod unnegate ((negation negation))
  (argument negation))

(defun negation? (thing)
  (typep thing 'negation))

(defmethod print-object ((neg negation) stream)
  (format stream "¬"))

(defgeneric negate (thing))

(defmethod negate ((formula formula))
  (make-instance 'negation :argument formula))

(defclass multiple-arity-connective-formula (composite-formula)
  ((items :initarg :items
	  :accessor items
	  :type list)))

(defgeneric connective-unit (multiple-arity-connective-formula))

(defmethod print-object :around ((formula multiple-arity-connective-formula)
				 stream)
  (let ((items (items formula)))
    (if (null items)
	(format stream "~A" (connective-unit formula))
	(if (null (cdr items))
	    (format stream "~A" (car items))
	    (progn
	      (format stream "(")
	      (format stream "~A" (car items))
	      (loop for item in (cdr items)
		   do
		   (format stream " ")
		   (call-next-method)
		   (format stream " ~A" item))
	      (format stream ")"))))))

(defun implication? (thing)
  (typep thing 'implication))

(defmethod print-object ((implication implication) stream)
  (format stream "-->"))

(defgeneric make-implication (antecedent consequent))

(defmethod make-implication ((antecedent formula) (consequent formula))
  (make-instance 'implication
		 :lhs antecedent
		 :rhs consequent))

(defgeneric antecedent (formula))
(defgeneric consequent (formula))

(defmethod antecedent ((implication implication))
  (lhs implication))

(defmethod consequent ((implication implication))
  (rhs implication))

(defun equivalence? (thing)
  (typep thing 'equivalence))

(defmethod print-object ((equiv equivalence) stream)
  (format stream "<-->"))

(defun make-equivalence (lhs rhs)
  (make-instance 'equivalence
		 :lhs lhs
		 :rhs rhs))

;;; disjunctions

(defun binary-disjunction? (thing)
  (typep thing 'binary-disjunction))

(defmethod print-object ((bin-dis binary-disjunction) stream)
  (format stream "or"))

(defgeneric make-binary-disjunction (lhs rhs))

(defclass multiple-arity-disjunction (multiple-arity-connective-formula)
  nil)

(defmethod connective-unit ((mad multiple-arity-disjunction))
  (declare (ignore mad))
  top)

(defun multiple-arity-disjunction? (thing)
  (eql (class-of thing) 'multiple-arity-disjunction))

(defmethod print-object ((mad multiple-arity-disjunction) stream)
  (format stream "or"))

(defmethod make-binary-disjunction ((lhs formula) (rhs formula))
  (make-instance 'binary-disjunction
		 :lhs lhs
		 :rhs rhs))

(defgeneric make-multiple-arity-disjunction (&rest disjuncts))

(defmethod make-multiple-arity-disjunction (&rest disjuncts)
  (if disjuncts
      (if (cdr disjuncts)
	  (if (cddr disjuncts)
	      (make-instance 'multiple-arity-disjunction
			     :items disjuncts)
	      (car disjuncts)))
      top))

(defun binary-disjunction->multiple-arity-disjunction (binary-disjunction)
  (make-instance 'multiple-arity-disjunction
		 :items (list (lhs binary-disjunction)
			      (rhs binary-disjunction))))

(defun multiple-arity-disjunction->binary-disjunction (multiple-arity-disjunction)
  (let ((disjuncts (items multiple-arity-disjunction)))
    (if (null disjuncts)
	(make-instance 'binary-disjunction
		       :lhs top
		       :rhs top)
	(if (null (cdr disjuncts))
	    (make-instance 'binary-disjunction
			   :lhs (first disjuncts)
			   :rhs contradiction)
	    (labels ((make-disjunction (ds)
		       (if (null (cddr ds))
			   (make-binary-disjunction (first ds)
						    (second ds))
			   (make-binary-disjunction (first ds)
						    (make-disjunction (cdr ds))))))
	      (make-disjunction disjuncts))))))

;; conjunctions

(defun binary-conjunction? (thing)
  (typep thing 'binary-conjunction))

(defmethod print-object ((con binary-conjunction) stream)
  (format stream "and"))

(defclass multiple-arity-conjunction (multiple-arity-connective-formula)
  nil)

(defmethod connective-unit ((mac multiple-arity-conjunction))
  (declare (ignore mac))
  contradiction)

(defun multiple-arity-conjunction? (thing)
  (eql (class-of thing) 'multiple-arity-conjunction))

(defmethod print-object ((mac multiple-arity-conjunction) stream)
  (format stream "and"))

(defun make-binary-conjunction (lhs rhs)
  (make-instance 'binary-conjunction
		 :lhs lhs
		 :rhs rhs))

(defun make-multiple-arity-conjunction (&rest conjuncts)
  (if conjuncts
      (if (cdr conjuncts)
	  (if (cddr conjuncts)
	      (make-instance 'multiple-arity-conjunction
			     :items conjuncts))
	  (cadr conjuncts))
      contradiction))

(defun binary-conjunction->multiple-arity-conjunction (binary-conjunction)
  (make-instance 'multiple-arity-conjunction
		 :items (list (lhs binary-conjunction)
			      (rhs binary-conjunction))))

(defun multiple-arity-conjunction->binary-conjunction (multiple-arity-conjunction)
  (let ((conjuncts (items multiple-arity-conjunction)))
    (if (null conjuncts)
	(make-binary-conjunction contradiction contradiction)
	(if (null (cdr conjuncts))
	    (make-instance 'binary-conjunction
			   :lhs (first conjuncts)
			   :rhs top)
	    (labels ((make-conjunction (ds)
		       (if (null (cddr ds))
			   (make-binary-conjunction (first ds)
						    (second ds))
			   (make-binary-conjunction (first ds)
						    (make-conjunction (cdr ds))))))
	      (make-conjunction conjuncts))))))

(defmethod print-object :after ((gen generalization) stream)
  (call-next-method)
  (format stream
	  "~A[~A]"
	  (bound-variable gen)
	  (matrix gen)))

(defun universal-generalization? (thing)
  (eql (class-of thing) 'universal-generalization))

(defmethod print-object ((uni-gen universal-generalization) stream)
  (format stream "all"))

(defun existential-generalization? (thing)
  (eql (class-of thing) 'existential-generalization))

(defmethod print-object ((exi-gen existential-generalization) stream)
  (format stream "exists"))

(defun make-universal (var formula)
  (make-instance 'universal-generalization
		 :bound-variable var
		 :matrix formula))

(defun make-existential (var formula)
  (make-instance 'existential-generalization
		 :bound-variable var
		 :matrix formula))

(defun equation? (formula)
  (eq (car formula) '=))

(defgeneric form->formula (thing)
  (:documentation "Given THING, try to make sense of it as a formula."))

(defgeneric op-and-args->formula (operator arguments)
  (:documentation "Try to understand a symbol OPERATOR and a list ARGUMENTS as a formula."))

(defgeneric op-and-args->term (operator arguments)
  (:documentation "Try to understand a symbol OPERATOR and a list ARGUMENTS as a term."))

(defmethod op-and-args->formula ((op (eql 'or)) arguments)
  (if (null arguments)
      (error 'parse-form-empty-argument-list-error :operator op)
      (if (null (cdr arguments))
	  (error 'parse-form-at-least-two-args-expected-but-only-one-supplied-error
		 :operator op
		 :first-arg (car arguments))
	  (if (null (cddr arguments))
	      (let ((first-disjunct (form->formula (car arguments)))
		    (second-disjunct (form->formula (cadr arguments))))
		(make-binary-disjunction first-disjunct second-disjunct))
	      (let ((disjuncts (mapcar #'form->formula arguments)))
		(apply #'make-multiple-arity-disjunction disjuncts))))))

(defmethod op-and-args->formula ((op (eql 'and)) arguments)
  (if (null arguments)
      (error 'parse-form-empty-argument-list-error :operator op)
      (if (null (cdr arguments))
	  (error 'parse-form-at-least-two-args-expected-but-only-one-supplied-error
		 :operator op)
	  (if (null (cddr arguments))
	      (let ((first-conjunct (form->formula (car arguments)))
		    (second-conjunct (form->formula (cadr arguments))))
		(make-binary-conjunction first-conjunct second-conjunct))
	      (let ((conjuncts (mapcar #'form->formula arguments)))
		(apply #'make-multiple-arity-conjunction conjuncts))))))

(defmethod op-and-args->formula ((op (eql 'not)) arguments)
  (if (null arguments)
      (error 'parse-form-empty-argument-list-error :operator op)
      (if (null (cdr arguments))
	  (let ((negated (form->formula (car arguments))))
	    (negate negated))
	  (error 'parse-form-unary-operator-multiple-arguments-error
		 :operator op))))

(defmethod op-and-args->formula ((op (eql 'implies)) arguments)
  (if (null arguments)
      (error 'parse-form-empty-argument-list-error :operator op)
      (if (null (cdr arguments))
	  (error 'parse-form-at-least-two-args-expected-but-only-one-supplied-error
		 :operator op)
	  (if (null (cddr arguments))
	      (let ((antecedent (form->formula (car arguments)))
		    (consequent (form->formula (cadr arguments))))
		(make-implication antecedent consequent))
	      (error 'parse-form-exactly-two-args-expected-but-at-least-three-supplied-error
		     :operator op
		     :args arguments)))))

(defmethod op-and-args->formula ((op (eql 'iff)) arguments)
  (if (null arguments)
      (error 'parse-form-empty-argument-list-error :operator op)
      (if (null (cdr arguments))
	  (error 'parse-form-at-least-two-args-expected-but-only-one-supplied-error
		 :operator op)
	  (if (null (cddr arguments))
	      (let ((lhs (form->formula (car arguments)))
		    (rhs (form->formula (cadr arguments))))
		(make-equivalence lhs rhs))
	      (error 'parse-form-exactly-two-args-expected-but-at-least-three-supplied-error
		     :operator op)))))

(defmethod op-and-args->formula ((op (eql 'all)) arguments)
  (if (null arguments)
      (error 'parse-form-empty-argument-list-error :operator op)
      (if (null (cdr arguments))
	  (error 'parse-form-at-least-two-args-expected-but-only-one-supplied-error
		 :operator op)
	  (if (null (cddr arguments))
	      (let ((var (form->term (car arguments))))
		(if (eql (class-of var) 'unsorted-variable-term)
		    (let ((matrix (form->formula (cadr arguments))))
		      (if (eql (class-of matrix) 'formula)
			  (make-universal var matrix)
			  (error 'parse-form-formula-expected-error
				 :operator op
				 :form (cadr arguments))))
		    (error 'parse-form-variable-expected
			   :operator op
			   :form (car arguments))))
	      (error 'parse-form-exactly-two-args-expected-but-at-least-three-supplied-error
		     :operator op)))))

(defmethod op-and-args->formula ((op (eql 'exists)) arguments)
  (if (null arguments)
      (error 'parse-form-empty-argument-list-error :operator op)
      (if (null (cdr arguments))
	  (error 'parse-form-at-least-two-args-expected-but-only-one-supplied-error
		 :operator op)
	  (if (null (cddr arguments))
	      (let ((var (form->formula (car arguments))))
		(if (eql (class-of var) 'unsorted-variable-term)
		    (let ((matrix (form->term (cadr arguments))))
		      (if (eql (class-of matrix) 'formula)
			  (make-universal var matrix)
			  (error 'parse-form-formula-expected-error
				 :operator op
				 :form (cadr arguments))))
		    (error 'parse-form-variable-expected
			   :operator op
			   :form (car arguments))))
	      (error 'parse-form-exactly-two-args-expected-but-at-least-three-supplied-error
		     :operator op)))))

(defmethod form->formula ((list list))
  (if (null list)
      (error 'parse-form-empty-list-supplied-error)
      (let ((first (first list)))
	(op-and-args->formula (symbolify-here first)
			      (cdr list)))))

(defmethod op-and-args->term ((op symbol) arguments)
  (let ((arguments-as-terms (mapcar #'form->term arguments)))
    (apply #'make-function-term
	   op
	   arguments-as-terms)))

(defmethod form->formula ((sym symbol))
  (make-atomic-formula (symbolify-here sym)))

(defgeneric form->term (form)
  (:documentation "Attempt to understand FORM as a term."))

(defmethod form->term ((list list))
  (if (null list)
      (error 'parse-form-empty-list-supplied-error)
      (op-and-args->term (symbolify-here (car list))
			 (cdr list))))

(defmethod form->term ((sym symbol))
  (let ((name (symbol-name sym)))
    (if (empty-string? name)
	(error 'parse-form-empty-string-supplied)
	(let ((first-char (char name 0)))
	  (if (char= first-char #\?)
	      (make-instance 'variable
			     :name (subseq name 1))
	      (make-function-term name))))))

(defmethod negate ((sym symbol))
  (negate (form->formula sym)))

(defmethod make-implication ((antecedent symbol) (consequent symbol))
  (make-implication (form->formula antecedent)
		    (form->formula consequent)))

(defmethod make-implication ((antecedent symbol) (consequent formula))
  (make-implication (form->formula antecedent)
		    consequent))

(defmethod make-implication ((antecedent formula) (consequent symbol))
  (make-implication antecedent
		    (form->formula consequent)))

(defmethod make-binary-disjunction ((lhs symbol) (rhs symbol))
  (make-binary-disjunction (form->formula lhs)
			   (form->formula rhs)))

(defmethod make-binary-disjunction ((lhs symbol) (rhs formula))
  (make-binary-disjunction (form->formula lhs)
			   rhs))

(defmethod make-binary-disjunction ((lhs formula) (rhs symbol))
  (make-binary-disjunction lhs
			   (form->formula rhs)))

(define-condition non-atomic-formula-error (error)
  ((text :initarg :text
	 :reader non-atomic-formula-error-text))
  (:report (lambda (condition stream)
	     (let ((text (non-atomic-formula-error-text condition)))
	       (if (null text)
		   (format stream
			   "Weird: no text was given (or text is simply NIL)")
		   (format stream
			   "The given text,~%~%  ~A,~%~%is an atomic formula."
			   text))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro -> (antecdent consequent)
  `(make-implication ,antecdent ,consequent))

(defmacro & (lhs rhs)
  `(make-binary-conjunction ,lhs ,rhs))

(defmacro v (lhs rhs)
  `(make-binary-disjunction ,lhs ,rhs))

(defmacro neg (argument)
  `(negate ,argument))

;;; formulas.lisp ends here