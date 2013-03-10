
(in-package :tipi)

(defparameter *szs-statuses* nil)

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
    :type list
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

(defmethod initialize-instance :after ((status szs-status) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (pushnew status *szs-statuses*
	   :test #'string=
	   :key #'short-name))

(defclass szs-success-status (szs-status)
  nil)

(defparameter *szs-success-statuses* nil)

(defmethod initialize-instance :after ((status szs-success-status) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (pushnew status *szs-success-statuses*
	   :test #'string=
	   :key #'short-name))

(defclass szs-no-success-status (szs-status)
  nil)

(defparameter *szs-statuses* nil)

(defparameter *szs-success-statuses* nil)

(defparameter *szs-no-success-statuses* nil)

(defmacro def-szs-success-status (symbol short-name long-name)
  `(defparameter ,symbol (make-instance 'szs-success-status
					:short-name ,short-name
					:long-name ,long-name)))

(defmacro def-szs-no-success-status (symbol short-name long-name)
  `(defparameter ,symbol (make-instance 'szs-no-success-status
					:short-name ,short-name
					:long-name ,long-name)))

(defgeneric szs-isa (szs-status-1 szs-status-2))

(defmethod szs-isa ((szs-status-1 szs-success-status)
		    (szs-status-2 szs-success-status))
  (find szs-status-1
	(isa-list szs-status-2)))

(defmethod szs-isa ((szs-status-1 szs-success-status)
		    (szs-status-2 szs-no-success-status))
  nil)

(defmethod szs-isa ((szs-status-1 szs-no-success-status)
		    (szs-status-2 szs-success-status))
  nil)

(defmethod szs-isa ((szs-status-1 szs-no-success-status)
		    (szs-status-2 szs-no-success-status))
  (find szs-status-1
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
(def-szs-success-status *szs-suc* "SUC" "Success")
(def-szs-success-status *szs-unp* "UNP" "UnsatisfiabilityPreserving")
(def-szs-success-status *szs-sap* "SAP" "SatisfiabilityPreserving")
(def-szs-success-status *szs-esa* "ESA" "EquiSatisfiable")
(def-szs-success-status *szs-sat* "SAT" "Satisfiable")
(def-szs-success-status *szs-fsa* "FSA" "FinitelySatisfiable")
(def-szs-success-status *szs-thm* "THM" "Theorem")
(def-szs-success-status *szs-eqv* "EQV" "Equivalent")
(def-szs-success-status *szs-tac* "TAC" "TautologousConclusion")
(def-szs-success-status *szs-wec* "WEC" "WeakerConclusion")
(def-szs-success-status *szs-eth* "ETH" "EquivalentTheorem")
(def-szs-success-status *szs-tau* "TAU" "Tautology")
(def-szs-success-status *szs-wtc* "WTC" "WeakerTautologousConclusion")
(def-szs-success-status *szs-wth* "WTH" "WeakerTheorem")
(def-szs-success-status *szs-cax* "CAX" "ContradictoryAxioms")
(def-szs-success-status *szs-sca* "SCA" "SatisfiableConclusionContradictoryAxioms")
(def-szs-success-status *szs-tca* "TCA" "TautologousConclusionContradictoryAxioms")
(def-szs-success-status *szs-wca* "WCA" "WeakerConclusionContradictoryAxioms")
(def-szs-success-status *szs-cup* "CUP" "CounterUnsatisfiabilityPreserving")
(def-szs-success-status *szs-csp* "CSP" "CounterSatisfiabilityPreserving")
(def-szs-success-status *szs-ecs* "ECS" "EquiCounterSatisfiable")
(def-szs-success-status *szs-csa* "CSA" "CounterSatisfiable")
(def-szs-success-status *szs-cth* "CTH" "CounterTheorem")
(def-szs-success-status *szs-ceq* "CEQ" "CounterEquivalent")
(def-szs-success-status *szs-unc* "UNC" "UnsatisfiableConclusion")
(def-szs-success-status *szs-wcc* "WCC" "WeakerCounterConclusion")
(def-szs-success-status *szs-ect* "ECT" "EquivalentCounterTheorem")
(def-szs-success-status *szs-fun* "FUN" "FinitelyUnsatisfiable")
(def-szs-success-status *szs-uns* "UNS" "Unsatisfiable")
(def-szs-success-status *szs-wuc* "WUC" "WeakerUnsatisfiableConclusion")
(def-szs-success-status *szs-wct* "WCT" "WeakerCounterTheorem")
(def-szs-success-status *szs-noc* "NOC" "NoConsequence")

;; No-success
(def-szs-no-success-status *szs-nos* "NOS" "NoSuccess")
(def-szs-no-success-status *szs-opn* "OPN" "Open")
(def-szs-no-success-status *szs-unk* "UNK" "Unknown")
(def-szs-no-success-status *szs-stp* "STP" "Stopped")
(def-szs-no-success-status *szs-err* "ERR" "Error")
(def-szs-no-success-status *szs-ose* "OSE" "OSError")
(def-szs-no-success-status *szs-ine* "INE" "InputError")
(def-szs-no-success-status *szs-sye* "SYE" "SyntaxError")
(def-szs-no-success-status *szs-see* "SEE" "SemanticError")
(def-szs-no-success-status *szs-tye* "TYE" "TypeError")
(def-szs-no-success-status *szs-for* "FOR" "Forced")
(def-szs-no-success-status *szs-usr* "USR" "User")
(def-szs-no-success-status *szs-rso* "RSO" "ResourceOut")
(def-szs-no-success-status *szs-tmo* "TMO" "Timeout")
(def-szs-no-success-status *szs-mmo* "MMO" "MemoryOut")
(def-szs-no-success-status *szs-gup* "GUP" "GaveUp")
(def-szs-no-success-status *szs-inc* "INC" "Incomplete")
(def-szs-no-success-status *szs-iap* "IAP" "Inappropriate")
(def-szs-no-success-status *szs-inp* "INP" "InProgress")
(def-szs-no-success-status *szs-ntt* "NTT" "NotTried")
(def-szs-no-success-status *szs-nty* "NTY" "NotTriedYet")

(defgeneric lookup-szs-status (szs-thing))

(defmethod lookup-szs-status ((status szs-status))
  status)

(defmethod lookup-szs-status ((szs-string string))
  (or (find szs-string *szs-statuses* :test #'string= :key #'short-name)
      (find szs-string *szs-statuses* :test #'string= :key #'long-name)))

(defmethod lookup-szs-status ((szs-symbol symbol))
  (lookup-szs-status (symbol-name szs-symbol)))

(defgeneric is-szs-success? (szs-thing))

(defmethod is-szs-success? ((szs-string string))
  (or (find szs-string *szs-success-statuses* :test #'string= :key #'short-name)
      (find szs-string *szs-success-statuses* :test #'string= :key #'long-name)))

(defmethod is-szs-success? ((status szs-status))
  (not (null (find status *szs-success-statuses*))))

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

(defun some-theorem? (szs-status-list)
  (some #'(lambda (status)
	    (szs-implies? status (lookup-szs-status "Theorem")))
	szs-status-list))
