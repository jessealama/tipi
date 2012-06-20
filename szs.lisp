
(in-package :tipi)

(defparameter *szs-short-names* (make-hash-table :test #'equal))

(defparameter *szs-long-names* (make-hash-table :test #'equal))

(defparameter *szs-success-statuses* (list))

(defparameter *szs-no-success-statuses* (list))

(defparameter *szs-statuses* (make-hash-table :test #'equal))

(defmacro define-szs-success-status (short-name long-name)
  (let ((status (gensym)))
    `(let ((,status (make-instance 'szs-success-status
			    :short-name ,short-name
			    :long-name ,long-name)))
       (setf (gethash ,short-name *szs-statuses*) ,status
	     (gethash ,short-name *szs-long-names*) ,long-name
	     (gethash ,long-name *szs-short-names*) ,short-name)
       (pushnew ,status *szs-success-statuses*
		:test #'string=
		:key #'short-name)
       ,status)))

(defmacro define-szs-non-success-status (short-name long-name)
  (let ((status (gensym)))
    `(let ((,status (make-instance 'szs-no-success-status
			      :short-name ,short-name
			      :long-name ,long-name)))
       (setf (gethash ,short-name *szs-statuses*) ,status
	     (gethash ,short-name *szs-long-names*) ,long-name
	     (gethash ,long-name *szs-short-names*) ,short-name)
       (pushnew ,status *szs-no-success-statuses*
		:test #'string=
		:key #'short-name)
       ,status)))

(defclass szs-status ()
  ((short-name
    :type string
    :initarg :short-name
    :reader short-name)
   (long-name
    :type string
    :initarg :long-name
    :reader long-name)
   (isa
    :type list
    :initarg :isa
    :accessor isa-list)
   (nota
    :type :list
    :initarg :nota
    :accessor nota-list)
   (nevera
    :type list
    :initarg :nevera
    :accessor nevera-list)
   (xora
    :type list
    :initarg :xora
    :accessor xora-list)))

(defmethod print-object ((status szs-status) stream)
  (print-unreadable-object (status stream :type t :identity nil)
    (format stream "~a (~a)" (short-name status) (long-name status))))

(defclass szs-success-status (szs-status)
  nil)

(defclass szs-no-success-status (szs-status)
  nil)

(defgeneric szs-isa (szs-status-1 szs-status-2))

(defmethod szs-isa ((szs-status-1 szs-success-status)
		    (szs-status-2 szs-success-status))
  (member szs-status-1
	  (isa-list szs-status-2)))

(defmethod szs-isa ((szs-status-1 szs-success-status)
		    (szs-status-2 szs-no-success-status))
  nil)

(defmethod szs-isa ((szs-status-1 szs-no-success-status)
		    (szs-status-2 szs-success-status))
  nil)

(defmethod szs-isa ((szs-status-1 szs-no-success-status)
		    (szs-status-2 szs-no-success-status))
  (member szs-status-1
	  (isa-list szs-status-2)))

(defgeneric szs-implies? (szs-status-1 szs-status-2))

(defmethod szs-implies? ((status-1 string) status-2)
  (szs-implies? (lookup-szs-status status-1) status-2))

(defmethod szs-implies? (status-1 (status-2 string))
  (szs-implies? status-1 (lookup-szs-status status-2)))

(defmethod szs-implies? ((szs-status-1 szs-status)
			(szs-status-2 szs-status))
  (or (eql szs-status-1 szs-status-2)
      (szs-isa szs-status-1 szs-status-2)))

;; Success
(defparameter *szs-suc* (define-szs-success-status "SUC" "Success"))
(defparameter *szs-unp* (define-szs-success-status "UNP" "UnsatisfiabilityPreserving"))
(defparameter *szs-sap* (define-szs-success-status "SAP" "SatisfiabilityPreserving"))
(defparameter *szs-esa* (define-szs-success-status "ESA" "EquiSatisfiable"))
(defparameter *szs-sat* (define-szs-success-status "SAT" "Satisfiable"))
(defparameter *szs-fsa* (define-szs-success-status "FSA" "FinitelySatisfiable"))
(defparameter *szs-thm* (define-szs-success-status "THM" "Theorem"))
(defparameter *szs-eqv* (define-szs-success-status "EQV" "Equivalent"))
(defparameter *szs-tac* (define-szs-success-status "TAC" "TautologousConclusion"))
(defparameter *szs-wec* (define-szs-success-status "WEC" "WeakerConclusion"))
(defparameter *szs-eth* (define-szs-success-status "ETH" "EquivalentTheorem"))
(defparameter *szs-tau* (define-szs-success-status "TAU" "Tautology"))
(defparameter *szs-wtc* (define-szs-success-status "WTC" "WeakerTautologousConclusion"))
(defparameter *szs-wth* (define-szs-success-status "WTH" "WeakerTheorem"))
(defparameter *szs-cax* (define-szs-success-status "CAX" "ContradictoryAxioms"))
(defparameter *szs-sca* (define-szs-success-status "SCA" "SatisfiableConclusionContradictoryAxioms"))
(defparameter *szs-tca* (define-szs-success-status "TCA" "TautologousConclusionContradictoryAxioms"))
(defparameter *szs-wca* (define-szs-success-status "WCA" "WeakerConclusionContradictoryAxioms"))
(defparameter *szs-cup* (define-szs-success-status "CUP" "CounterUnsatisfiabilityPreserving"))
(defparameter *szs-csp* (define-szs-success-status "CSP" "CounterSatisfiabilityPreserving"))
(defparameter *szs-ecs* (define-szs-success-status "ECS" "EquiCounterSatisfiable"))
(defparameter *szs-csa* (define-szs-success-status "CSA" "CounterSatisfiable"))
(defparameter *szs-cth* (define-szs-success-status "CTH" "CounterTheorem"))
(defparameter *szs-ceq* (define-szs-success-status "CEQ" "CounterEquivalent"))
(defparameter *szs-unc* (define-szs-success-status "UNC" "UnsatisfiableConclusion"))
(defparameter *szs-wcc* (define-szs-success-status "WCC" "WeakerCounterConclusion"))
(defparameter *szs-ect* (define-szs-success-status "ECT" "EquivalentCounterTheorem"))
(defparameter *szs-fun* (define-szs-success-status "FUN" "FinitelyUnsatisfiable"))
(defparameter *szs-uns* (define-szs-success-status "UNS" "Unsatisfiable"))
(defparameter *szs-wuc* (define-szs-success-status "WUC" "WeakerUnsatisfiableConclusion"))
(defparameter *szs-wct* (define-szs-success-status "WCT" "WeakerCounterTheorem"))
(defparameter *szs-noc* (define-szs-success-status "NOC" "NoConsequence"))

;; No-success
(defparameter *szs-nos* (define-szs-non-success-status "NOS" "NoSuccess"))
(defparameter *szs-opn* (define-szs-non-success-status "OPN" "Open"))
(defparameter *szs-unk* (define-szs-non-success-status "UNK" "Unknown"))
(defparameter *szs-stp* (define-szs-non-success-status "STP" "Stopped"))
(defparameter *szs-err* (define-szs-non-success-status "ERR" "Error"))
(defparameter *szs-ose* (define-szs-non-success-status "OSE" "OSError"))
(defparameter *szs-ine* (define-szs-non-success-status "INE" "InputError"))
(defparameter *szs-sye* (define-szs-non-success-status "SYE" "SyntaxError"))
(defparameter *szs-see* (define-szs-non-success-status "SEE" "SemanticError"))
(defparameter *szs-tye* (define-szs-non-success-status "TYE" "TypeError"))
(defparameter *szs-for* (define-szs-non-success-status "FOR" "Forced"))
(defparameter *szs-usr* (define-szs-non-success-status "USR" "User"))
(defparameter *szs-rso* (define-szs-non-success-status "RSO" "ResourceOut"))
(defparameter *szs-tmo* (define-szs-non-success-status "TMO" "Timeout"))
(defparameter *szs-mmo* (define-szs-non-success-status "MMO" "MemoryOut"))
(defparameter *szs-gup* (define-szs-non-success-status "GUP" "GaveUp"))
(defparameter *szs-inc* (define-szs-non-success-status "INC" "Incomplete"))
(defparameter *szs-iap* (define-szs-non-success-status "IAP" "Inappropriate"))
(defparameter *szs-inp* (define-szs-non-success-status "INP" "InProgress"))
(defparameter *szs-ntt* (define-szs-non-success-status "NTT" "NotTried"))
(defparameter *szs-nty* (define-szs-non-success-status "NTY" "NotTriedYet"))

(defgeneric lookup-szs-status (szs-thing))

(defmethod lookup-szs-status ((status szs-status))
  status)

(defmethod lookup-szs-status ((szs-string string))
  (multiple-value-bind (status found?)
      (gethash szs-string *szs-statuses*)
    (if found?
	status
	(multiple-value-bind (short-name short-name-known?)
	    (gethash szs-string *szs-short-names*)
	  (if short-name-known?
	      (lookup-szs-status short-name)
	      (error "Unknown SZS status '~a'." szs-string))))))

(defmethod lookup-szs-status ((szs-symbol symbol))
  (lookup-szs-status (symbol-name szs-symbol)))

(defgeneric is-szs-success? (szs-thing))

(defmethod is-szs-success? ((szs-string string))
  (multiple-value-bind (ok ok?)
      (gethash (lookup-szs-status szs-string) *szs-success-statuses*)
    (declare (ignore ok))
    ok?))

(defmethod is-szs-success? ((status szs-status))
  (member status *szs-success-statuses*))

;;; Relations among SZS statuses

;; Success isa relationships
(setf (isa-list *szs-unp*) nil)
(setf (isa-list *szs-sap*) nil)
(setf (isa-list *szs-esa*) (list *szs-unp* *szs-sap*))
(setf (isa-list *szs-sat*) (list *szs-unp* *szs-sap* *szs-esa*))
(setf (isa-list *szs-thm*) (list *szs-sap*))
(setf (isa-list *szs-eqv*) (list *szs-unp* *szs-sap* *szs-esa* *szs-sat* *szs-thm*))
(setf (isa-list *szs-tac*) (list *szs-unp* *szs-sap* *szs-esa* *szs-sat* *szs-thm*))
(setf (isa-list *szs-wec*) (list *szs-unp* *szs-sap* *szs-esa* *szs-sat* *szs-thm*))
(setf (isa-list *szs-eth*) (list *szs-unp* *szs-sap* *szs-esa* *szs-sat* *szs-thm* *szs-eqv*))
(setf (isa-list *szs-tau*) (list *szs-unp* *szs-sap* *szs-esa* *szs-sat* *szs-thm* *szs-eqv* *szs-tac*))
(setf (isa-list *szs-wtc*) (list *szs-unp* *szs-sap* *szs-esa* *szs-sat* *szs-thm* *szs-tac* *szs-wec*))
(setf (isa-list *szs-wth*) (list *szs-unp* *szs-sap* *szs-esa* *szs-sat* *szs-thm* *szs-wec*))
(setf (isa-list *szs-cax*) (list *szs-sap* *szs-thm*))
(setf (isa-list *szs-sca*) (list *szs-sap* *szs-thm* *szs-cax*))
(setf (isa-list *szs-tca*) (list *szs-sap* *szs-thm* *szs-cax* *szs-sca*))
(setf (isa-list *szs-wca*) (list *szs-sap* *szs-thm* *szs-cax* *szs-sca*))
(setf (isa-list *szs-csa*) (list *szs-unp*))
(setf (isa-list *szs-uns*) (list *szs-unp* *szs-csa*))
(setf (isa-list *szs-noc*) (list *szs-unp* *szs-sap* *szs-esa* *szs-sat* *szs-csa*))
;; No success isa relationships
(setf (isa-list *szs-nos*) nil)
(setf (isa-list *szs-opn*) (list *szs-nos*))
(setf (isa-list *szs-unk*) (list *szs-nos*))
(setf (isa-list *szs-stp*) (list *szs-unk* *szs-nos*))
(setf (isa-list *szs-inp*) (list *szs-unk* *szs-nos*))
(setf (isa-list *szs-ntt*) (list *szs-unk* *szs-nos*))
(setf (isa-list *szs-err*) (list *szs-stp* *szs-unk* *szs-nos*))
(setf (isa-list *szs-for*) (list *szs-stp* *szs-unk* *szs-nos*))
(setf (isa-list *szs-gup*) (list *szs-stp* *szs-unk* *szs-nos*))
(setf (isa-list *szs-nty*) (list *szs-ntt* *szs-unk* *szs-nos*))
(setf (isa-list *szs-ose*) (list *szs-err* *szs-stp* *szs-unk* *szs-nos*))
(setf (isa-list *szs-ine*) (list *szs-err* *szs-stp* *szs-unk* *szs-nos*))
(setf (isa-list *szs-usr*) (list *szs-for* *szs-stp* *szs-unk* *szs-nos*))
(setf (isa-list *szs-rso*) (list *szs-for* *szs-gup* *szs-stp* *szs-unk* *szs-nos*))
(setf (isa-list *szs-inc*) (list *szs-gup* *szs-stp* *szs-unk* *szs-nos*))
(setf (isa-list *szs-iap*) (list *szs-gup* *szs-stp* *szs-ntt* *szs-unk* *szs-nos*))
(setf (isa-list *szs-sye*) (list *szs-ine* *szs-err* *szs-stp* *szs-unk* *szs-nos*))
(setf (isa-list *szs-see*) (list *szs-ine* *szs-err* *szs-stp* *szs-unk* *szs-nos*))
(setf (isa-list *szs-tmo*) (list *szs-rso* *szs-for* *szs-gup* *szs-stp* *szs-unk* *szs-nos*))
(setf (isa-list *szs-mmo*) (list *szs-rso* *szs-for* *szs-gup* *szs-stp* *szs-unk* *szs-nos*))
(setf (isa-list *szs-tye*) (list *szs-see* *szs-ine* *szs-err* *szs-stp* *szs-unk* *szs-nos*))

;; Success not-a relationships
(setf (nota-list *szs-unp*) (list *szs-sap* *szs-esa* *szs-sat* *szs-thm* *szs-eqv* *szs-tac* *szs-wec* *szs-eth* *szs-tau* *szs-wtc* *szs-wth* *szs-cax* *szs-csa* *szs-uns* *szs-noc*))
(setf (nota-list *szs-sap*) (list *szs-unp* *szs-esa* *szs-sat* *szs-thm* *szs-eqv* *szs-tac* *szs-wec* *szs-eth* *szs-tau* *szs-wtc* *szs-wth* *szs-cax* *szs-sca* *szs-tca* *szs-wca* *szs-csa* *szs-noc*))
(setf (nota-list *szs-esa*) (list *szs-sat* *szs-thm* *szs-eqv* *szs-tac* *szs-wec* *szs-eth* *szs-tau* *szs-wtc* *szs-wth* *szs-cax* *szs-csa* *szs-noc*))
(setf (nota-list *szs-sat*) (list *szs-thm* *szs-eqv* *szs-tac* *szs-wec* *szs-eth* *szs-tau* *szs-wtc* *szs-wth* *szs-csa* *szs-noc*))
(setf (nota-list *szs-thm*) (list *szs-unp* *szs-esa* *szs-sat* *szs-eqv* *szs-tac* *szs-wec* *szs-eth* *szs-tau* *szs-wtc* *szs-wth* *szs-cax* *szs-sca* *szs-tca* *szs-wca*))
(setf (nota-list *szs-eqv*) (list *szs-tac* *szs-eth* *szs-tau*))
(setf (nota-list *szs-tac*) (list *szs-eqv* *szs-wec* *szs-tau* *szs-wtc*))
(setf (nota-list *szs-wec*) (list *szs-tac* *szs-wtc* *szs-wth*))
(setf (nota-list *szs-eth*) nil)
(setf (nota-list *szs-tau*) nil)
(setf (nota-list *szs-wtc*) nil)
(setf (nota-list *szs-wth*) nil)
(setf (nota-list *szs-cax*) (list *szs-unp* *szs-esa* *szs-sca* *szs-tca* *szs-wca*))
(setf (nota-list *szs-sca*) (list *szs-tca* *szs-wca*))
(setf (nota-list *szs-tca*) (list *szs-eqv*))
(setf (nota-list *szs-wca*) nil)
(setf (nota-list *szs-csa*) (list *szs-sap* *szs-esa* *szs-sat* *szs-uns* *szs-noc*))
(setf (nota-list *szs-uns*) nil)
(setf (nota-list *szs-noc*) nil)

;; Success never-a relationships
(setf (nevera-list *szs-unp*) (list *szs-tca* *szs-wca*))
(setf (nevera-list *szs-sap*) (list *szs-uns*))
(setf (nevera-list *szs-esa*) (list *szs-sca* *szs-tca* *szs-wca* *szs-uns*))
(setf (nevera-list *szs-sat*) (list *szs-cax* *szs-sca* *szs-tca* *szs-wca* *szs-uns*))
(setf (nevera-list *szs-thm*) (list *szs-uns* *szs-noc*))
(setf (nevera-list *szs-eqv*) (list *szs-wec* *szs-wtc* *szs-wth* *szs-cax* *szs-sca* *szs-tca* *szs-wca* *szs-csa* *szs-uns* *szs-noc*))
(setf (nevera-list *szs-tac*) (list *szs-eth* *szs-wth* *szs-cax* *szs-sca* *szs-tca* *szs-wca* *szs-csa* *szs-uns* *szs-noc*))
(setf (nevera-list *szs-wec*) (list *szs-eqv* *szs-eth* *szs-tau* *szs-cax* *szs-sca* *szs-tca* *szs-wca* *szs-csa* *szs-uns* *szs-noc*))
(setf (nevera-list *szs-eth*) (list *szs-tac* *szs-wec* *szs-tau* *szs-wtc* *szs-wth* *szs-cax* *szs-sca* *szs-tca* *szs-wca* *szs-csa* *szs-uns* *szs-noc*))
(setf (nevera-list *szs-tau*) (list *szs-wec* *szs-eth* *szs-wtc* *szs-wth* *szs-cax* *szs-sca* *szs-tca* *szs-wca* *szs-csa* *szs-uns* *szs-noc*))
(setf (nevera-list *szs-wtc*) (list *szs-eqv* *szs-eth* *szs-tau* *szs-wth* *szs-cax* *szs-sca* *szs-tca* *szs-wca* *szs-csa* *szs-uns* *szs-noc*))
(setf (nevera-list *szs-wth*) (list *szs-eqv* *szs-tac* *szs-eth* *szs-tau* *szs-wtc* *szs-cax* *szs-sca* *szs-tca* *szs-wca* *szs-csa* *szs-uns* *szs-noc*))
(setf (nevera-list *szs-cax*) (list *szs-sat* *szs-eqv* *szs-tac* *szs-wec* *szs-eth* *szs-tau* *szs-wtc* *szs-wth* *szs-csa* *szs-uns* *szs-noc*))
(setf (nevera-list *szs-sca*) (list *szs-esa* *szs-sat* *szs-eqv* *szs-tac* *szs-wec* *szs-eth* *szs-tau* *szs-wtc* *szs-wth* *szs-csa* *szs-uns* *szs-noc*))
(setf (nevera-list *szs-tca*) (list *szs-unp* *szs-esa* *szs-sat* *szs-tac* *szs-wec* *szs-eth* *szs-tau* *szs-wtc* *szs-wth* *szs-wca* *szs-csa* *szs-uns* *szs-noc*))
(setf (nevera-list *szs-wca*) (list *szs-unp* *szs-esa* *szs-sat* *szs-eqv* *szs-tac* *szs-wec* *szs-eth* *szs-tau* *szs-wtc* *szs-wth* *szs-tca* *szs-csa* *szs-uns* *szs-noc*))
(setf (nevera-list *szs-csa*) (list *szs-eqv* *szs-tac* *szs-wec* *szs-eth* *szs-tau* *szs-wtc* *szs-wth* *szs-cax* *szs-sca* *szs-tca* *szs-wca*))
(setf (nevera-list *szs-uns*) (list *szs-sap* *szs-esa* *szs-sat* *szs-thm* *szs-eqv* *szs-tac* *szs-wec* *szs-eth* *szs-tau* *szs-wtc* *szs-wth* *szs-cax* *szs-sca* *szs-tca* *szs-wca* *szs-noc*))
(setf (nevera-list *szs-noc*) (list *szs-thm* *szs-eqv* *szs-tac* *szs-wec* *szs-eth* *szs-tau* *szs-wtc* *szs-wth* *szs-cax* *szs-sca* *szs-tca* *szs-wca* *szs-uns*))

;; Success xora relationships
(setf (nevera-list *szs-unp*) (list *szs-sca*))
(setf (nevera-list *szs-thm*) (list *szs-csa*))
(setf (nevera-list *szs-sca*) (list *szs-unp*))
(setf (nevera-list *szs-csa*) (list *szs-thm*))

(defun strongest-szs-status (status-list)
  (when status-list
    (remove-if #'(lambda (status)
		   (some #'(lambda (other-status)
			     (and (not (eql status other-status))
				  (szs-implies? other-status status)))
			 status-list))
	       status-list)))

(defun aggregate-szs-statuses (statuses)
  (let ((successes (remove-if-not #'is-szs-success? statuses)))
    (if successes
	(first (strongest-szs-status successes))
	(first (strongest-szs-status statuses)))))
