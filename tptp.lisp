
(in-package :tipi)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass tptp-formula ()
  ((name
    :initarg :name
    :initform (error "An fof needs a name.")
    :accessor name)
   (role
    :initarg :role
    :accessor role
    :initform (error "An fof needs a role."))
   (formula
    :initarg :formula
    :accessor formula
    :initform (error "An fof needs a formula."))
   (source
    :initarg :source
    :accessor source)
   (optional-info
    :initarg :optional-info
    :accessor optional-info)))

(defmethod initialize-instance :after ((x tptp-formula) &rest initargs &key &allow-other-keys)
  "Ensure that if X's optional-info slot is set, then its source slot is also set."
  (declare (ignore initargs))
  (when (slot-boundp x 'optional-info)
    (unless (slot-boundp x 'source)
      (error "A TPTP formula whose optional-info slot is bound must also have its source slot bound."))))

(defclass fof (tptp-formula)
  nil)

(defclass cnf (tptp-formula)
  nil)

(defclass internal-source (tptp-source)
  nil)

(defclass external-source (tptp-source)
  nil)

(defmethod print-object ((x tptp-formula) stream)
  (with-slots (name role formula)
      x
    (if (slot-boundp x 'source)
	(let ((source (source x)))
	  (if (slot-boundp x 'optional-info)
	      (let ((optional-info (optional-info x)))
		(format stream "(~a, ~a, ~a, ~a, ~a)." name role formula source optional-info))
	      (format stream "(~a, ~a, ~a, ~a)." name role formula source)))
	(format stream "(~a, ~a, ~a)." name role formula))))

(defmethod print-object ((x fof) stream)
  (format stream "fof")
  (call-next-method))

(defmethod print-object ((x cnf) stream)
  (format stream "fof")
  (call-next-method))

(defgeneric render (tptp-thing)
  (:documentation "A plain text rendering of TPTP-THING."))

(defgeneric render-html (tptp-thing session)
  (:documentation "An HTML rendering of TPTP-THING for the hunchentoot session SESSION."))

(defmethod render ((formula fof))
  (format nil "fof(~a,~a,~a)."
	  (name formula)
	  (role formula)
	  (formula formula)))



(defmethod render-html ((x tptp-formula) session)
  (with-slots (name role formula)
      x
    (let ((rendered-formula (render-html formula session)))
      (register-groups-bind (sans-outer-parens)
	  ("^[(](.+)[)]$" rendered-formula)
	(setf rendered-formula sans-outer-parens))
      (with-html-output-to-string (dummy)
	((:td :class "formula-name")
	 (fmt "~a" name))
	((:td :class (format nil "~a" role)))
	((:td :class "formula-proper")
	 (fmt "~a" rendered-formula))
	(if (slot-boundp x 'source)
	    (htm ((:td :class "formula-source")
		  (fmt "~a" (render-html (source x) session))))
	    (htm ((:td :class "formula-source"))))
	(if (slot-boundp x 'optional-info)
	    (htm ((:td :class "formula-optional-info")
		  (fmt "~a" (render-html (optional-info x) session))))
	    (htm ((:td :class "formula-optional-info"))))))))

(defmethod render-html ((fof fof) session)
  (with-html-output-to-string (dummy)
    ((:tr :id (format nil "~a" (name fof))
	  :class "fof")
     (call-next-method))))

(defmethod render-html ((x cnf) session)
  (with-html-output-to-string (dummy)
    ((:tr :id (format nil "~a" (name x))
	  :class "cnf")
     (fmt "~a" (call-next-method)))))

(defmethod render ((formula cnf))
  (format nil "cnf(~a,~a,~a)."
	  (name formula)
	  (role formula)
	  (formula formula)))

(defgeneric make-tptp-formula (thing))

(defmethod make-tptp-formula ((thing list))
  (destructuring-bind (syntax name status formula . more-stuff)
      thing
    (if more-stuff
	(destructuring-bind (source . useful-info)
	    more-stuff
	  (make-instance 'tptp-formula
		   :name (if (symbolp name)
			     (symbol-name name)
			     (format nil "~a" name))
		   :syntax (symbol-name syntax)
		   :status (symbol-name status)
		   :formula (form->formula formula)
		   :source source
		   :useful-info useful-info))
	(make-instance 'tptp-formula
		   :name (if (symbolp name)
			     (symbol-name name)
			     (format nil "~a" name))
		   :syntax (symbol-name syntax)
		   :status (symbol-name status)
		   :formula (form->formula formula)))))

(defun sort-formula-list (formula-list)
  (let ((sorted (sort formula-list #'string< :key #'name)))
    (mapcar #'name sorted)))

(defparameter *tptp-to-lisp-stylesheet*
  #p"/Users/alama/sources/xsl4tptp/tptp-to-lisp.xsl")

(defgeneric xmlize-tptp (tptp))

(defmethod xmlize-tptp :around ((tptp-file pathname))
  (if (probe-file tptp-file)
      (call-next-method)
      (error "There is no file at '~a'." (namestring tptp-file))))

(defmethod xmlize-tptp ((tptp-file pathname))
  (let ((tptp4X-out (make-string-output-stream))
	(tptp4X-err (make-string-output-stream))
	(tptp-dir (pathname (directory-namestring tptp-file))))
    (with-current-directory (tptp-dir)
      (run-program "tptp4X"
		   (list "-c" "-x" "-fxml" "--")
		   :wait t
		   :output tptp4X-out
		   :error tptp4X-err
		   :input tptp-file))
    (prog1
	(get-output-stream-string tptp4X-out)
      (close tptp4X-out)
      (close tptp4X-err))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TPTP databases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass tptp-db ()
  ((formulas :type list
	     :initarg :formulas
	     :accessor formulas
	     :initform nil)
   (path
    :type (or null pathname)
    :accessor path
    :initform nil
    :initarg :path)))

(defmethod print-object ((problem tptp-db) stream)
  (format stream "~{~a~%~}" (formulas problem)))

(defun problem-directory (tptp-db)
  (with-slots (path)
      tptp-db
    (when (pathnamep path)
      (directory-namestring path))))

(defmethod signature ((formula tptp-formula))
  (signature (formula formula)))

(defmethod signature ((tptp tptp-db))
  (reduce #'merge-signatures
	  (mapcar #'signature
		  (mapcar #'formula
			  (formulas (expand-includes tptp))))))

(defclass derivability-problem (tptp-db)
  ((conjecture
    :initarg :conjecture
    :accessor conjecture
    :initform (error "To specify a derivability problem, a conjecture must be supplied."))))

(defmethod print-object ((problem derivability-problem) stream)
  (print-unreadable-object (problem stream :type t :identity nil)
    (let ((conjecture (conjecture problem))
	  (formulas (formulas problem)))
      (format stream "Conjecture: ~a" conjecture)
      (if formulas
	  (format stream "Premises:~%~{~a~%~}" formulas)
	  (format stream "Premises: (none)")))))

(defmethod initialize-instance :after ((problem derivability-problem) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (when (some #'(lambda (premise)
		  (string= (role premise) "conjecture"))
	      (formulas problem))
    (error "Some non-conjecture formula has the TPTP status 'conjecture'."))
  (let ((conjecture (conjecture problem)))
    (unless (string= (role conjecture) "conjecture")
      (setf (conjecture problem) (change-status conjecture "conjecture"))))
  problem)

(defgeneric make-derivability-problem (formulas))

(defmethod make-derivability-problem ((formulas tptp-db))
  (let ((conjecture (conjecture-formula formulas)))
    (if conjecture
	(make-instance 'derivability-problem
		       :formulas (non-conjecture-formulas formulas)
		       :conjecture conjecture
		       :path (path formulas))
	(error "There is no conjecture formula in ~a." formulas))))

(defmethod make-derivability-problem ((formulas null))
  (error "The empty list does not contain a conjecture formula."))

(defmethod make-derivability-problem ((formulas list))
  (let ((conjecture (find "conjecture" formulas :test #'string= :key #'role))
	(non-conjecture-formulas (remove-if #'(lambda (formula)
						(string= (role formula) "conjecture"))
					    formulas)))
    (if conjecture
	(make-instance 'derivability-problem
		       :formulas non-conjecture-formulas
		       :conjecture conjecture)
	(error "No conjecture formula found in ~{~a~%~}" formulas))))

(defmethod make-derivability-problem ((problem pathname))
  (make-derivability-problem (parse-tptp problem)))

(defmethod render ((formulas list))
  (if formulas
      (format nil "~{~a~%~}" (mapcar #'render formulas))
      (format nil "(empty formula list)")))

(defmethod render ((problem tptp-db))
  (render (formulas problem)))

(defmethod render-html ((formula-list null) session)
  "")

(defmethod render-html ((formula-list list) session)
  (with-html-output-to-string (dummy)
    ((:table :class "tptp-db" :title "TPTP formulas")
     (:caption "TPTP formulas")
     (:thead
      (:tr
       (:th "Name")
       (:th "Role")
       (:th "Formula")
       (:th "Source")
       (:th "Optional Info")))
     (:tbody
      (dolist (formula formula-list)
	(htm (fmt "~a" (render-html formula session)))))
     (:tfoot
      (:tr
       ((:td :colspan 3)
	(:p ((:span :class "conjecture") "Conjectures")
	    ", "
	    ((:span :class "definition") "Definitions")
	    ", "
	    ((:span :class "axiom") "Axioms")
	    ", "
	    ((:span :class "lemma") "Lemmas")
	    ", "
	    ((:span :class "hypothesis") "Hypotheses")
	    ", "
	    ((:span :class "plain") "Plain"))))))))

(defmethod render-html ((problem tptp-db) session)
  (with-slots (formulas)
      problem
    (with-html-output-to-string (dummy)
      (let ((includes (remove-if-not #'(lambda (x) (eql (type-of x) 'include-instruction)) formulas))
	    (non-includes (remove-if #'(lambda (x) (eql (type-of x) 'include-instruction)) formulas)))
	(when includes
	  (htm
	   ((:table :title "Include statements" :class "include-table")
	    (:caption "Include statements")
	    (:thead
	     (:tr
	      (:th "File")
	      (:th "Selection")))
	    (dolist (include includes)
	      (htm (:tbody
		    (fmt "~a" (render-html include session)))))
	    (:tfoot
	     ((:form :method "post"
		     :action "expand"
		     :enctype "multipart/form-data")
	      ((:input :type "submit"
		       :title "Expand these include statements"
		       :value "Expand")))))))
	(let ((session-problem (session-value :problem session))
	      (session-solution (session-value :solution session)))
	  (cond ((eq problem session-problem)
		 (htm (fmt "~a" (render-html non-includes session))))
		((eq problem session-solution)
		 (let ((solution-properties (session-value :solution-properties session)))
		   (let ((restrict-signature (gethash "restrict-signature" solution-properties))
			 (kowalski (gethash "kowalski" solution-properties))
			 (squeeze-quantifiers (gethash "squeeze-quantifiers" solution-properties))
			 (supporting-axioms (gethash "supporting-axioms" solution-properties))
			 (reduce-equivalences (gethash "reduce-equivalences" solution-properties)))
		     (let ((reformulated non-includes))
		       (when restrict-signature
			 (when (not (solution-restricted-p problem))
			   (setf reformulated (restrict-solution-to-problem-language problem))))
		       (when kowalski
			 (setf reformulated (kowalski reformulated)))
		       (when squeeze-quantifiers
			 (setf reformulated (squeeze-quantifiers reformulated)))
		       (when supporting-axioms
			 (setf reformulated (supporting-axioms (if (typep reformulated 'tptp-db)
								   reformulated
								   problem))))
		       (when reduce-equivalences
			 (setf reformulated (reduce-equivalences reformulated)))
		       (setf (session-value :solution session)
			     (if (typep reformulated 'tstp-db)
				 reformulated
				 (make-instance 'tstp-db
						:problem session-problem
						:restricted nil
						:formulas (if (typep reformulated 'list)
							      reformulated
							      (if (typep reformulated 'tptp-db)
								  (formulas reformulated)
								  (error "huh?"))))))
		       (htm (fmt "~a" (render-html reformulated session)))))))
		(t
		 (htm (fmt "~a" (render-html non-includes session))))))))))

(defmethod kowalski ((l null))
  nil)

(defmethod kowalski ((l list))
  (mapcar #'kowalski l))

(defmethod render ((problem derivability-problem))
  (with-output-to-string (s)
    (dolist (formula (formulas problem))
      (format s "~a" (render formula))
      (terpri s))
    (format s "~a" (render (conjecture problem)))
    (terpri s)))

(defgeneric proper-formulas (problem))

(defmethod proper-formulas ((problem tptp-db))
  (mapcar #'formula (formulas problem)))

(defun conjecture-formula (problem)
  (has-conjecture-formula? problem))

(defun has-conjecture-formula? (problem)
  (first (member "conjecture"
		 (formulas problem)
		 :key #'role
		 :test #'string=)))

(defun conjecture-string? (string)
  (string= string "conjecture"))

(defun remove-conjecture (problem)
  (make-instance 'tptp-db
		 :formulas (remove-if #'conjecture-string?
				      (formulas problem)
				      :key #'role)))

(defgeneric remove-formula (formulas formula))

(defmethod remove-formula ((formulas tptp-db) (formula-name string))
  "Remove any formula in FORMULAS whose name is FORMULA-NAME."
  (make-instance 'tptp-db
		 :formulas (remove-if #'(lambda (x) (string= x formula-name))
				      (formulas formulas)
				      :key #'name)))

(defmethod remove-formula ((problem derivability-problem) (formula tptp-formula))
  (let ((name-to-remove (name formula))
	(conjecture-name (name (conjecture problem))))
    (if (string= name-to-remove conjecture-name)
	(make-instance 'tptp-db
		       :formulas (formulas problem))
	(make-instance 'derivability-problem
		       :conjecture (conjecture problem)
		       :formulas (remove-if #'(lambda (x) (string= x name-to-remove))
					    (formulas problem)
					    :key #'name)))))

(defmethod remove-formula ((formulas tptp-db) (formula tptp-formula))
  (remove-formula formulas (name formula)))

(defun formulas-with-status (problem status)
  (remove-if-not #'(lambda (stat) (string= stat status))
		 (formulas problem)
		 :key #'role))

(defun statuses-of-formulas (problem)
  (loop
     with statuses = (make-hash-table :test #'equal)
     for formula in (formulas problem)
     for status = (role formula)
     do (setf (gethash status statuses) 0)
     finally (return (hash-table-keys statuses))))

(defun non-conjecture-formulas (problem)
  (remove-if #'(lambda (stat) (string= stat "conjecture"))
	     (formulas problem)
	     :key #'role))

(defgeneric change-status (formula new-status))

(defmethod change-status :around ((formula tptp-formula) status)
  (let ((new-formula (call-next-method)))
    (when (slot-boundp formula 'source)
      (setf (source new-formula) (source formula)))
    (when (slot-boundp formula 'optional-info)
      (setf (optional-info new-formula)
	    (optional-info formula)))
    new-formula))

(defmethod change-status ((formula tptp-formula) new-status)
  (make-instance (class-of 'tptp-formula)
		 :name (name formula)
		 :syntax (role formula)
		 :status new-status
		 :formula (formula formula)))

(defgeneric change-status-of-formula-in (formula problem new-status)
  (:documentation "Change the TPTP status of FORMULA in PROBLEM to NEW-STATUS."))

(defmethod change-status-of-formula-in ((formula string)
					(problem pathname)
					(new-status string))
  (change-status-of-formula-in formula (parse-tptp problem) new-status))

(defmethod change-status-of-formula-in ((formula string)
					(problem tptp-db)
					(new-status string))
  (let ((formula-in-problem (formula-with-name problem formula)))
    (if formula-in-problem
	(let ((other-formulas (remove-if #'(lambda (name)
					     (string= name formula))
					 (formulas problem)
					 :key #'name)))
	  (let ((new-formula (change-status formula-in-problem new-status)))
	    (make-instance 'tptp-db
			   :formulas (cons new-formula
					   other-formulas)))))))

(defmethod change-status-of-formula-in ((formula string)
					(problem derivability-problem)
					(new-status string))
  (if (string= new-status "conjecture")
      (let ((conjecture (conjecture problem)))
	(let ((conjecture-name (name conjecture)))
	  (if (string= conjecture-name formula)
	      problem
	      (error "The given derivability-problem already has a conjecture formula; (by the name ~a), so we cannot change the status of ~a into 'conjecture'." conjecture-name formula))))
      (let ((formula-in-problem (formula-with-name problem formula)))
    (if formula-in-problem
	(let ((other-formulas (remove-if #'(lambda (name)
					     (string= name formula))
					 (formulas problem)
					 :key #'name)))
	  (let ((new-formula (change-status formula-in-problem new-status)))
	    (make-instance 'tptp-db
			   :formulas (cons new-formula
					   other-formulas))))))))

(defun promote-conjecture-to-axiom (problem)
  (let ((conjecture (has-conjecture-formula? problem)))
    (if conjecture
	(make-instance 'tptp-db
		       :formulas (cons (change-status conjecture "axiom")
				       (non-conjecture-formulas problem)))
	problem)))

(defun formula-names (tptp-db)
  (mapcar #'name (formulas tptp-db)))

(defgeneric formula-with-name (tptp-db name))

(defmethod formula-with-name ((tptp-db tptp-db) (name integer))
  (formula-with-name tptp-db (format nil "~d" name)))

(defmethod formula-with-name ((tptp-db tptp-db) (name symbol))
  (formula-with-name tptp-db (symbol-name name)))

(defmethod formula-with-name ((tptp-db tptp-db) (name string))
  (first (remove-if-not #'(lambda (x) (string= x name))
			(formulas-w/o-includes tptp-db)
			:key #'(lambda (x) (format nil "~a" (name x))))))

(defgeneric premises (problem))

(defmethod premises ((db tptp-db))
  (non-conjecture-formulas db))

(defgeneric restrict-to (db formulas)
  (:documentation "Restrict DB to the formulas in FORMULAS."))

(defmethod restrict-to ((db tptp-db) (formulas list))
  (let ((new-formulas nil))
    (dolist (formula formulas)
      (cond ((stringp formula)
	     (let ((formula-in-db (formula-with-name db formula)))
	       (when formula-in-db
		 (push formula-in-db new-formulas))))
	    ((typep formula 'formula)
	     (let ((formula-in-db (formula-with-name db (name formula))))
	       (when formula-in-db
		 (push formula-in-db new-formulas))))
	    (t
	     (error "Don't know how to handle ~a." formula))))
    (make-instance 'tptp-db
		   :formulas new-formulas)))

(defmethod restrict-to ((problem derivability-problem) (formulas list))
  (let* ((new-formulas nil)
	 (conjecture (conjecture problem))
	 (conjecture-name (name conjecture)))
    (dolist (formula formulas)
      (cond ((stringp formula)
	     (let ((formula-in-db (formula-with-name problem formula)))
	       (when formula-in-db
		 (unless (string= formula conjecture-name)
		   (push formula-in-db new-formulas)))))
	    ((typep formula 'formula)
	     (let ((formula-in-db (formula-with-name problem (name formula))))
	       (when formula-in-db
		 (unless (string= (name formula) conjecture-name)
		   (push formula-in-db new-formulas)))))
	    (t
	     (error "Don't know how to handle ~a." formula))))
    (make-instance 'derivability-problem
		   :conjecture conjecture
		   :formulas new-formulas)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Includes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass include-instruction ()
  ((file
    :accessor file
    :initarg :file
    :initform (error "An include instruction requires a file name."))
   (selection
    :accessor selection
    :initarg :selection
    :type list
    :initform nil)))

(defmethod print-object ((include include-instruction) stream)
  (print-unreadable-object (include stream :type t :identity nil)
    (format stream "~a : (~{~a~^ ~})" (file include) (selection include))))

(defmethod render ((include include-instruction))
  (with-slots (file selection)
      include
    (format nil "include(~a,[~{~a~^,~}])." file selection)))

(defmethod render-html ((include include-instruction) session)
  (with-slots (file selection)
      include
    (with-html-output-to-string (dummy)
      (:tr
       ((:td :class "file-name") (fmt "~a" file))
       (if (null selection)
	   (htm ((:td :class "formula-selection") "(none)"))
	   (htm ((:td :class "formula-selection")
		 (loop
		    :with len = (length selection)
		    :for formula :in selection
		    :for i :from 1
		    :do
		    (htm ((:span :class "formula-name") (fmt "~a" formula)))
		    (when (< i len)
		      (htm (str ", ")))))))))))

(defgeneric simplify-justification (tptp))

(defmethod simplify-justification ((tptp-string string))
  (simplify-justification (parse-tptp tptp-string)))

(defmethod simplify-justification ((tptp-path pathname))
  (simplify-justification (parse-tptp tptp-path)))

(defmethod simplify-justification ((tptp-db tptp-db))
  (let* ((formulas (formulas tptp-db))
	 (new-formulas nil)
	 (names (mapcar #'name formulas))
	 (names-table (make-hash-table :test #'equal)))
    (dolist (name names)
      (setf (gethash name names-table) t))
    (dolist (formula formulas)
      (if (slot-boundp formula 'source)
	  (let* ((source (source formula))
		 (earlier-table (make-hash-table :test #'equal))
		 (atoms (flatten-tptp source)))
	    (dolist (atom atoms)
	      (when (gethash atom names-table)
		(unless (gethash atom earlier-table)
		  (setf (gethash atom earlier-table) t))))
	    (let ((new-source (make-instance 'general-list
					     :terms (hash-table-keys earlier-table))))
	      (let ((new-formula (make-instance (class-of formula)
						:name (name formula)
						:role (role formula)
						:formula (formula formula)
						:source new-source)))
		(when (slot-boundp formula 'optional-info)
		  (setf (optional-info new-formula)
			(optional-info formula)))
		(push new-formula new-formulas))))
	  (let ((new-formula (make-instance (class-of formula)
					    :name (name formula)
					    :role (role formula)
					    :formula (formula formula))))
	    (setf (source new-formula)
		  (make-instance 'general-list :terms nil))
	    (push new-formula new-formulas))))
    (make-instance 'tptp-db
		   :formulas (reverse new-formulas))))

(defmethod simplify-justification ((include include-instruction))
  include)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expanding includes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric expand-include (include root-dir))

(defmethod expand-include ((include include-instruction) root-dir)
  (with-slots (selection file)
      include
    (when (null selection)
      (return-from expand-include nil))
    (let ((real-file nil)
	  (new-formulas nil))
      (cond ((file-exists-p file)
	     (setf real-file file))
	    ((directory-exists-p root-dir)
	     (let ((file-in-dir (merge-pathnames file root-dir)))
	       (if (file-exists-p file-in-dir)
		   (setf real-file file-in-dir)
		   (error "No file found at '~a', nor could we find '~a'." (namestring file) (namestring file-in-dir)))))
	    ((null root-dir)
	     (error "No file at '~a', and no root directory was supplied." (namestring file)))
	    (t
	     (error "No file at '~a', and to boot a bogus directory (~a) was supplied." (namestring file) root-dir)))
      (let ((included-db (handler-case (parse-tptp real-file)
			   (error () nil))))
	(unless included-db
	  (error "Error parsing '~a' as a TPTP file." (namestring real-file)))
	(setf included-db (expand-includes included-db))
	(loop
	   for selected in selection
	   for corresponding = (formula-with-name included-db
						  selected)
	   do
	     (when (null corresponding)
	       (error "There is no formula in the TPTP problem '~a' with the name '~a'." (namestring real-file) selected))
	     (push corresponding new-formulas)
	   finally
	     (return (reverse new-formulas)))))))

(defgeneric expand-includes (tptp))

(defmethod expand-includes ((tptp-db pathname))
  (expand-includes (parse-tptp tptp-db)))

(defmethod expand-includes ((tptp-db tptp-db))
  (let ((new-formulas nil)
	(path (path tptp-db))
	(dir (problem-directory tptp-db)))
    (if (null (include-instructions tptp-db))
	tptp-db
	(loop
	   for formula in (formulas tptp-db)
	   do
	     (if (eql (type-of formula) 'include-instruction)
		 (let ((expanded (expand-include formula dir)))
		   (dolist (x expanded)
		     (push x new-formulas)))
		 (push formula new-formulas))
	   finally
	     (return (make-instance 'tptp-db
				    :formulas (reverse new-formulas)
				    :path path))))))

(defun include-instructions (tptp-db)
  (remove-if-not #'(lambda (x) (eql (type-of x) 'include-instruction)) (formulas tptp-db)))

(defun formulas-w/o-includes (tptp-db)
  (remove-if #'(lambda (x) (eql (type-of x) 'include-instruction)) (formulas tptp-db)))

(defmethod kowalski :around ((formula tptp-formula))
  (let ((new-formula (call-next-method)))
    (when (slot-boundp formula 'source)
      (setf (source new-formula)
	    (source formula)))
    (when (slot-boundp formula 'optional-info)
      (setf (optional-info new-formula)
	    (optional-info formula)))
    new-formula))

(defmethod kowalski ((formula tptp-formula))
  (make-instance (class-of formula)
		 :name (name formula)
		 :role (role formula)
		 :formula (kowalski (formula formula))))

(defmethod kowalski ((db tptp-db))
  (make-instance 'tptp-db
		 :path (path db)
		 :formulas (mapcar #'kowalski (formulas db))))

(defmethod squeeze-quantifiers ((db tptp-db))
  (make-instance 'tptp-db
		 :path (path db)
		 :formulas (mapcar #'squeeze-quantifiers (formulas db))))

(defmethod squeeze-quantifiers :around ((formula tptp-formula))
  (let ((new-formula (call-next-method)))
    (when (slot-boundp formula 'source)
      (setf (source new-formula)
	    (source formula)))
    (when (slot-boundp formula 'optional-info)
      (setf (optional-info new-formula)
	    (optional-info formula)))
    new-formula))

(defmethod squeeze-quantifiers ((formula tptp-formula))
  (make-instance (class-of formula)
		 :name (name formula)
		 :role (role formula)
		 :formula (squeeze-quantifiers (formula formula))))

(defgeneric supporting-axioms (tptp))

(defun exists-path (from to table)
  (if (string= from to)
      t
      (let ((predecessors (gethash from table)))
	(some #'(lambda (x) (exists-path x to table))
	      predecessors))))

(defmethod supporting-axioms ((x inference-record))
  (supporting-axioms (parents x)))

(defmethod supporting-axioms ((x general-list))
  (reduce #'append (mapcar #'supporting-axioms (terms x))))

(defmethod supporting-axioms ((x string))
  (list x))

(defmethod supporting-axioms ((x integer))
  (list (format nil "~d" x)))

(defgeneric axioms (tptp))

(defmethod axioms ((db tptp-db))
  (remove-if-not #'axiom-p (formulas db)))

(defgeneric axiom-p (formula))

(defmethod axiom-p ((formula tptp-formula))
  (if (slot-boundp formula 'source)
      (let ((source (source formula)))
	(cond ((typep source 'internal-source)
	       nil)
	      ((typep source 'general-list)
	       (null (terms source)))
	      ((integerp source)
	       nil)
	      (t
	       (error "Don't know how to determine whether '~a' is an axiom." formula))))
      t))

(defmethod supporting-axioms ((db tptp-db))
  (let ((support-table (make-hash-table :test #'equal)))
    (dolist (formula (formulas db))
      (let ((name (stringify (name formula))))
	(if (slot-boundp formula 'source)
	    (setf (gethash name support-table)
		  (supporting-axioms (source formula)))
	    (setf (gethash name support-table) nil))))
    (flet ((axiom-p (name) (null (gethash name support-table))))
      (let ((keys (hash-table-keys support-table)))
	(let ((axioms (remove-if-not #'axiom-p keys))
	      (non-axioms (remove-if #'axiom-p keys)))
	  (let ((full-table (make-hash-table :test #'equal)))
	    (dolist (axiom axioms)
	      (setf (gethash axiom full-table) nil))
	    (dolist (non-axiom non-axioms)
	      (flet ((supports (axiom)
		       (exists-path non-axiom axiom support-table)))
		(setf (gethash non-axiom full-table)
		      (remove-if-not #'supports axioms))))
	    (flet ((update-formula (formula)
		     (setf (optional-info formula)
		     (make-instance 'general-list
				    :terms (gethash (stringify (name formula))
						    full-table)))))
	      (dolist (formula (formulas db))
		(update-formula formula)))))))))

(defgeneric easily-equivalent (formula-1 formula-2)
  (:documentation "Can we prove that FORMULA-1 and FORMULA-2 are
  logically equivalent (in first-order classical logic with identity)
  in one second with the E theorem prover?"))

(defmethod easily-equivalent ((formula-1 tptp-formula) (formula-2 tptp-formula))
  (easily-equivalent (formula formula-1)
		     (formula formula-2)))

(defmethod easily-equivalent ((formula-1 string) formula-2)
  (easily-equivalent (parse-tptp formula-1)
		     formula-2))

(defmethod easily-equivalent (formula-1 (formula-2 string))
  (easily-equivalent formula-1 (parse-tptp formula-2)))

(defmethod easily-equivalent (formula-1 (formula-2 tptp-db))
  (easily-equivalent formula-1
		     (make-instance 'multiple-arity-conjunction
				    :items (mapcar #'formula (formulas formula-2)))))

(defmethod easily-equivalent ((formula-1 tptp-db) formula-2)
  (easily-equivalent (make-instance 'multiple-arity-conjunction
				    :items (mapcar #'formula (formulas formula-1)))
		     formula-2))

(defparameter *easily-equivalent-timeout* 5
  "The maximum number of seconds we will spend trying to prove that two formulas are equivalent.")

(defmethod easily-equivalent ((formula-1 formula) (formula-2 formula))
  (let ((formula-1-closed (universally-close formula-1))
	(formula-2-closed (universally-close formula-2)))
    (let ((temp-problem (temporary-file :base "problem"))
	  (temp-output (temporary-file :base "output")))
      (write-string-into-file (format nil "fof(equivalent,conjecture,(~a <=> ~a)).~%" formula-1-closed formula-2-closed)
			      temp-problem)
      (run-program "eproof"
		   (list "--auto"
			 "--tstp-format"
			 "--memory-limit=1024"
			 (format nil "--cpu-limit=~d" *easily-equivalent-timeout*)
			 (native-namestring temp-problem))
		   :search t
		   :input nil
		   :output temp-output
		   :wait t)
      (prog1
	  (not (null (scan "SZS status Theorem" (file-contents temp-output))))
	(when (file-exists-p temp-output)
	  (delete-file temp-output))
	(when (file-exists-p temp-problem)
	  (delete-file temp-problem))))))

(defgeneric fofify (tptp))

(defmethod fofify ((db tptp-db))
  (make-instance 'tptp-db
		 :path (path db)
		 :formulas (mapcar #'fofify (formulas db))))

(defmethod fofify :around ((formula tptp-formula))
  (let ((new-formula (call-next-method)))
    (when (slot-boundp formula 'source)
      (setf (source new-formula) (source formula)))
    (when (slot-boundp formula 'optional-info)
      (setf (optional-info new-formula)
	    (optional-info formula)))
    new-formula))

(defmethod fofify ((formula tptp-formula))
  (make-instance 'fof
		 :name (name formula)
		 :role (role formula)
		 :formula (universally-close (formula formula))))

(defgeneric premises (thing))

(defmethod premises ((x null))
  nil)

(defmethod premises ((x inference-record))
  (premises (parents x)))

(defmethod premises ((x null))
  nil)

(defmethod premises ((x general-list))
  (reduce #'append (mapcar #'premises (terms x))))

(defmethod premises ((x list))
  (reduce #'append (mapcar #'premises x)))

(defmethod premises ((x integer))
  (list x))

(defmethod premises ((x tptp-formula))
  (when (slot-boundp x 'source)
    (premises (source x))))

(defmethod premises ((x atomic-expression))
  nil)

(defun replace-premises (formula new-premises)
  (setf (source formula)
	(make-instance 'atomic-expression
		       :head (intern "inference" :tipi)
		       :arguments (list (make-instance 'atomic-expression
						       :head (intern "unknown" :tipi)
						       :arguments nil)
					(make-instance 'general-list)
					(make-instance 'general-list
						       :terms new-premises))))
  formula)

(defgeneric find-formula (formulas name))

(defmethod find-formula ((formulas null) name)
  nil)

(defmethod find-formula ((formulas list) name)
  (find (stringify name)
	formulas
	:key #'(lambda (x) (stringify (name x)))
	:test #'string=))

(defgeneric update-inference-parents (thing from to)
  (:documentation "In THING, update any reference to FROM in an
  inference record to TO."))

(defmethod update-inference-parents ((thing null) from to)
  nil)

(defmethod update-inference-parents ((l general-list) from to)
  (make-instance 'general-list
		 :terms (mapcar #'(lambda (term)
				    (update-inference-parents term from to))
				(terms l))))

(defmethod update-inference-parents ((formula-list list) from to)
  (mapcar #'(lambda (x)
	      (update-inference-parents x from to))
	  formula-list))

(defmethod update-inference-parents :around ((x tptp-formula) from to)
  (let ((new-formula (call-next-method)))
    (when (slot-boundp x 'optional-info)
      (setf (optional-info new-formula)
	    (optional-info x)))
    new-formula))

(defmethod update-inference-parents ((x tptp-formula) from to)
  (if (slot-boundp x 'source)
      (make-instance (class-of x)
		     :name (name x)
		     :role (role x)
		     :formula (formula x)
		     :source (update-inference-parents (source x) from to))
      x))

(defmethod update-inference-parents ((db tptp-db) from to)
  (make-instance (class-of db)
		 :path (path db)
		 :formulas (mapcar #'(lambda (x)
				       (update-inference-parents x from to))
				   (formulas db))))

(defmethod update-inference-parents ((x atomic-expression) from to)
  (make-instance (class-of x)
		 :head (head x)
		 :arguments (mapcar #'(lambda (arg)
					(update-inference-parents arg from to))
				    (arguments x))))

(defmethod update-inference-parents ((record inference-record) from to)
  (with-slots (rule useful-info parents)
      record
    (make-instance 'inference-record
		   :rule rule
		   :useful-info useful-info
		   :parents (make-instance 'general-list
					   :terms (mapcar #'(lambda (x)
							      (update-inference-parents x from to))
							  (terms parents))))))

(defmethod update-inference-parents ((x string) from to)
  (if (string= x (stringify from))
      to
      x))

(defmethod update-inference-parents ((x integer) from to)
  (if (string= (format nil "~d" x)
	       (stringify from))
      to
      x))

(defmethod update-inference-parents ((x symbol) from to)
  (if (string= (symbol-name x)
	       (stringify from))
      to
      x))

(defgeneric reduce-equivalences (tptp))

(defmethod reduce-equivalences ((l null))
  nil)

(defmethod reduce-equivalences ((l list))
  (multiple-value-bind (index-to-keep index-to-remove)
      (loop
	 :for i :from 0
	 :for formula :in l
	 :for rest = (subseq l (1+ i))
	 :for equivalent = (find-if #'(lambda (other-formula)
					(let ((premises (premises other-formula)))
					  (when (length= 1 premises)
					    (let ((premise (first premises)))
					      (when (string= (stringify premise)
							     (stringify (name formula)))
						(easily-equivalent formula other-formula))))))
				    rest)
	 :when equivalent :do (return (values i (position equivalent l)))
	 :finally (return (values nil nil)))
    (if (and index-to-keep index-to-remove)
	(reduce-equivalences (append (subseq l 0 index-to-remove)
				     (update-inference-parents (subseq l (1+ index-to-remove))
							       index-to-remove
							       index-to-keep)))
	l)))

(defmethod reduce-equivalences ((db tptp-db))
  (let ((simplified (simplify-justification db)))
    (make-instance 'tptp-db
		   :path (path db)
		   :formulas (reduce-equivalences (formulas simplified)))))
