;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: KR; Base: 10 -*-


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; eliminate ugly   (g-value-inherit-values schema slot T nil)
;      (setf entry (slot-accessor schema slot))
;    from gv-value-fn (in constraints.lisp)
;;; Merge formulas with structure slots.
;;; reuse directory-slot for formulas.
;;; reuse discarded slot structures in set-slot-accessor
;;; reuse discarded slot structures in clear-schema-slots


(in-package "KR")

(eval-when (eval load compile)
  (export '(SCHEMA
	    CREATE-INSTANCE CREATE-PROTOTYPE CREATE-RELATION CREATE-SCHEMA
	    FORMULA O-FORMULA
	    SCHEMA-P RELATION-P IS-A-P HAS-SLOT-P FORMULA-P
	    S-VALUE G-VALUE G-CACHED-VALUE G-LOCAL-VALUE GV GVL GV-LOCAL
	    GET-VALUE GET-LOCAL-VALUE
	    DOVALUES DOSLOTS
	    DEFINE-METHOD KR-SEND CALL-PROTOTYPE-METHOD APPLY-PROTOTYPE-METHOD
	    METHOD-TRACE
	    WITH-CONSTANTS-DISABLED WITH-TYPES-DISABLED
	    WITH-DEMONS-DISABLED WITH-DEMON-DISABLED WITH-DEMON-ENABLED
	    CHANGE-FORMULA MOVE-FORMULA RECOMPUTE-FORMULA COPY-FORMULA KR-PATH
	    MARK-AS-CHANGED MARK-AS-INVALID
	    PS CALL-ON-PS-SLOTS NAME-FOR-SCHEMA
	    DECLARE-CONSTANT SLOT-CONSTANT-P
	    DESTROY-SLOT DESTROY-SCHEMA DESTROY-CONSTRAINT
	    DEF-KR-TYPE G-TYPE S-TYPE CHECK-SLOT-TYPE KR-BOOLEAN
	    GET-TYPE-DOCUMENTATION SET-TYPE-DOCUMENTATION GET-TYPE-DEFINITION
	    GET-DECLARATIONS GET-SLOT-DECLARATIONS
	    G-FORMULA-VALUE S-FORMULA-VALUE
	    ;;; This should be exported but is not - LispWorks bug:
	    ;;; SELF-OLD-VALUE
	    )))


(defparameter *kr-version* "2.3.4")



(eval-when (eval compile load)
  (defvar *special-kr-optimization*
    '(optimize (speed 3) (space 0) #+(or ALLEGRO APPLE) (debug 0)))

  (proclaim '(special user::*default-garnet-proclaim*))
  (if (boundp 'user::*default-garnet-proclaim*)
    (if user::*default-garnet-proclaim*
      (proclaim user::*default-garnet-proclaim*))
    (proclaim '(optimize (safety 1) (space 0)
		(speed #-LUCID 3 #+LUCID 2) #+(or APPLE ALLEGRO) (debug 3))
	      #+COMMENT
	      '(optimize (safety 0) (space 0)
		(speed #-LUCID 3 #+LUCID 2) #+(or APPLE ALLEGRO) (debug 0)))))


;;; This enables the eager-evaluation version.
;;; 
#|
;;; Currently turned off.
(eval-when (eval load compile)
  (unless (find :lazy *features*)
    (pushnew :eager *features*)))
|#




;;; -------------------------------------------------- Internal structures.




;;; The internal representation of a schema is as a structure, where the
;;; <name> slot holds the name (or internal number) of the schema and the
;;; <slots> slot holds a p-list of slot names and slot values.
;;; 
(defstruct (schema (:predicate is-schema)
                   (:print-function print-the-schema))
  name      	; the schema name, or a number
  bins		; bins of lists of slots
  )

#|
(ts (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
      (schema-bins a)) 100000)

(defun foo (object)
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (schema-bins object)))
|#


;;; This structure is similar to a schema, but is used to store formulas.
;;; It prints out with an F instead of an S, and it uses the same positions for
;;; different functions.
;;; 
(defstruct (a-formula (:include schema) (:print-function print-the-schema))
  ;;; number	; valid/invalid bit, and sweep mark.  Actually stored in the
  		; structure slot "a-formula-bins", inherited from schema.
  depends-on	; list of schemata on which this function depends (or single
  		; schema if there is only one)
  schema	; schema on which this formula is installed
  slot		; slot on which this formula is installed
  cached-value	; the cached value
  path		; holds cached paths
  is-a		; parent formula, if any
  function	; executable formula function
  lambda	; the original lambda expression, if applicable
  is-a-inv	; list of formulas that inherit from this one
  meta		; NIL, or a KR schema that contains meta-information
  #+EAGER
  priority      ; formula's position in topological order
  #+EAGER
  bits          ; contains the valid/invalid, visited/not-visited,
                ; renumbered/not-renumbered, eval-q/not-eval-q, and
                ; cycle/non-cycle bits, as well as a count of the number
                ; of times the formula has been evaluated
  #+EAGER
  valid
  #+EAGER
  dfnumber      ; number assigned by depth-first search
  #+EAGER
  lowlink       ; lowest dfnumber of a node that this formula is linked to
  )


;;; The value in a slot is represented as a structure of this type.
;;;
(defstruct (sl (:print-function print-the-slot))
  name
  value
  bits)


;;; This is similar; it includes room to store dependent formulas.
;;;
(defstruct (full-sl (:include sl))
  dependents
  ;; demons
  )



;;; -------------------------------------------------- Variables, etc.


(eval-when (compile load eval)
  (defmacro defparam (&rest body)
    #+(or GARNET-DEBUG (not CLISP)) `(defparameter ,@body)
    ;; Get more speed out of clisp by using constants
    #+(and (not GARNET-DEBUG) CLISP) `(defconstant ,@body)
    ))

#+GARNET-BINS
(eval-when (compile load eval)
  (defparameter *bins-length* 8))


(eval-when (compile load eval)
  (defvar *store-lambdas* T
    "If NIL, lambda expressions are not stored in formulas"))


(defvar *types-enabled* T
  "Set to T to enable type checking on s-value and formula reevaluation")


(defvar *warning-on-create-schema* T
  "If nil, no warning is printed when create-schema is redefining an existing
  schema.")

(defvar *warning-on-circularity* nil
  "Set this to NIL to prevent warning when a circularity is detected.")

(defvar *warning-on-evaluation* nil
  "If non-NIL, a warning is printed every time a formula is reevaluated.
  This may be useful during debugging.")

(defvar *warning-on-null-link* NIL
  "If non-NIL, a warning is printed when a null link is evaluated inside a
  GV (or GVL) within a formula.  This is the case when the stale value of the
  formula is reused.")

(defvar *warning-on-disconnected-formula* T
  "If nil, no warning is printed when propagate-change sees a disconnected
  formula.")


(eval-when (compile load eval)
  (defvar *print-new-instances* T))


(eval-when (compile load eval)
  (defmacro a-local-only-slot (slot)
    `(eq ,slot :is-a-inv)))


(defvar *setting-formula-p* nil
  "Set to T only when we are setting a slot with a formula")


(defvar *within-g-value* nil
  "Set to non-nil within a sub-formula evaluation")


(defvar *sweep-mark* 0
  "Used as a sweep mark to detect circularities")


(defvar *demons-disabled* nil
  "May be bound to T to cause demons NOT to be executed when a slot is set.
  If the value is a single value, or a list, ")


(defvar *constants-disabled* NIL
  "May be bound to NIL to cause constant declarations to be ignore in
  create-instance.")


(defvar *redefine-ok* NIL
  "May be bound to T to allow create-instance to redefine slots that were
  declare constant in the prototype.")


(defvar *pre-set-demon* nil
  "May be bound to a function to be called as a slot is set in a schema
  with the slots new-value.")

(defvar *slot-setter-debug* nil
  "May be bound to a function of three arguments for debugging situations
   in which it is important to know when a slot is being set, either
   indirectly of via formula re-evaluation.  The function is called with
   the object, the slot name, and the new value.")

(defvar *schema-self* nil
  "The schema being acted upon by the accessor functions.")

(defvar *schema-slot* nil
  "The slot in *schema-self* being acted upon by the accessor functions.")

(defvar *current-formula* nil
  "The formula being acted upon by the accessor functions.")

(defvar *last-formula* nil
  "Similar to *current-formula*, used for debugging only.")


(defvar *inheritance-relations* '()
  "All relations in this list perform inheritance.")

(defvar *inheritance-inverse-relations* '()
  "Inverses of all relations which perform inheritance.")

(defvar *relations* '()
  "An a-list of relations known to the system, with their inverse(s).
   Used for the creation of automatic reverse-links.")


(defparameter *reuse-formulas* (make-array 1 :adjustable t :fill-pointer 0)
  "A list of formulas that have been destroyed and can be reused.  This
   avoids the need to allocate and deallocate formulas all the time.")

(defparameter *reuse-slots* (make-array 1 :adjustable t :fill-pointer 0)
  "An array of slot arrays that have been destroyed and can be reused.  This
   avoids the need to allocate and deallocate arrays all the time.")

(defparameter *reuse-directories* (make-array 1 :adjustable t :fill-pointer 0)
  "An array of directory arrays that have been destroyed and can be reused..")


(defvar *schema-is-new* nil
  "If non-nil, we are inside the creation of a new schema.  This guarantees
  that we do not have to search for inverse links when creating relations,
  and avoids the need to scan long is-a-inv lists.")


(defvar *print-as-structure* T
  "If non-nil, schema names are printed as structure references.")

(defvar *print-structure-slots* nil
  "List of slots that should be printed when printing schemata as structures.")


(defparam *no-value* '(:no-value)
  "A cons cell which is used to mark the value of non-existent slots.")


(defvar *schema-counter* 0
  "This variable is used to generate schema numbers for schemata that
  are created with (create-schema NIL).")


(eval-when (eval compile load)
  (defparam *type-bits* 10)  ;; # of bits for encoding type

  (defparam *type-mask* (1- (expt 2 *type-bits*))) ;; to extract type

  ;; bit is 1 if slot contains inherited values, 0 for local values
  (defparam *inherited-bit*          *type-bits*)
  ;; bit is 1 if any other schema inherited the value from here
  (defparam *is-parent-bit*          (1+ *inherited-bit*))
  (defparam *is-constant-bit*        (1+ *is-parent-bit*))
  (defparam *is-update-slot-bit*     (1+ *is-constant-bit*))
  (defparam *is-local-only-slot-bit* (1+ *is-update-slot-bit*))
  (defparam *is-parameter-slot-bit*  (1+ *is-local-only-slot-bit*)))


(eval-when (eval compile load)
  (defparam *local-mask* 0)
  (defparam *constant-mask* (ash 1 *is-constant-bit*))
  (defparam *is-update-slot-mask* (ash 1 *is-update-slot-bit*))
  (defparam *inherited-mask* (ash 1 *inherited-bit*))
  (defparam *is-parent-mask* (ash 1 *is-parent-bit*))

  (defparam *clear-slot-mask*
    (logior *local-mask* *type-mask* *constant-mask* *is-update-slot-mask*))

  (defparam *inherited-parent-mask*
    (logior *inherited-mask* *is-parent-mask*))
  (defparam *not-inherited-mask* (lognot *inherited-mask*))
  (defparam *not-parent-mask* (lognot *is-parent-mask*))
  (defparam *not-parent-constant-mask*
    (lognot (logior *is-parent-mask* *constant-mask*)))

  (defparam *all-bits-mask* (lognot *type-mask*)))


(defvar *check-constants* NIL
  "If T, first-time evaluation for the current formula.  Check whether it
   is a constant formula.")

(defvar *is-constant* T)

(defvar *accessed-slots* NIL
  "Tells whether any slot was accessed during formula evaluation")

(defvar *kr-send-self* nil
  "The current schema for kr-send.")

(defvar *kr-send-slot* nil
  "The current slot for kr-send.")

(defvar *kr-send-parent* nil
  "The schema from which the last prototype method was obtained.")

(defvar *create-schema-schema* nil
  "Name of the current object being defined by Create-Instance.  Used for
   debugging only.")



;;; --------------------------------------------------


;;; This macro will output the <forms> only if GARNET-DEBUG is defined.
;;;
(defmacro when-debug (&rest forms)
  #+GARNET-DEBUG
  `(progn ,@forms)
  #-GARNET-DEBUG
  (declare (ignore forms))
  #-GARNET-DEBUG
  nil)



;;; --------------------------------------------------



(defmacro formula-p (thing)
  `(a-formula-p ,thing))



;;; -------------------------------------------------- EAGER EVALUATION


;;; -------------------- Definitions of value-information bits.

#+EAGER
(eval-when (eval compile load)
  ;; bit is 1 if formula is part of a cycle, 0 otherwise
  (defparam *cycle-bit* 0)
  ;; bit is 1 if formula is on the evaluation queue, 0 otherwise
  (defparam *eval-bit* 1)
  ;; bit is 1 if the formula has been visited during a depth-first
  ;; search, 0 otherwise
  (defparam *visited-bit* 2)
  ;; bit is 1 if the formula's priority has been renumbered during the
  ;; renumbering of a cycle, 0 otherwise
  (defparam *renumber-bit* 3)
  ;; count keeps track of how many times the formula has been evaluated and
  ;; is called the formula's timestamp
  (defparam *fixed-bit* 4)
  ;; indicates if formula's value is fixed on this iteration of the constraint
  ;; solver and thus should not be reevaluated

  (defparam *count-bit* 5)
  (defparam *neg-count-bit* (- *count-bit*))

  ;;; Bits in a dependency structure.
  ;; bit is 1 if the dependency is part of a cycle, 0 otherwise
  (defparam *cycle-edge-bit* 0)
  ;; the status of a dependency is indicated by a timestamp. if the
  ;; timestamp is greater than or equal to the timestamp in the dependency's
  ;; formula, the dependency is valid; otherwise the dependency is invalid
  (defparam *status-bit* 1)
  (defparam *neg-status-bit* (- *status-bit*)))



#+EAGER
(eval-when (eval compile load)
  (defparam *cycle-mask* (ash 1 *cycle-bit*))
  (defparam *eval-mask* (ash 1 *eval-bit*))
  (defparam *visited-mask* (ash 1 *visited-bit*))
  (defparam *renumber-mask* (ash 1 *renumber-bit*))
  (defparam *fixed-mask* (ash 1 *fixed-bit*))
  (defparam *count-mask* (ash 1 *count-bit*))
  (defparam *status-mask* (ash 1 *status-bit*))
  (defparam *cycle-edge-mask* (ash 1 *cycle-edge-bit*)))



#+EAGER
(defvar *eval-queue* nil
  "Contains formulas to be evaluated")


#+EAGER
(defvar *eval-count* 0
  "Number of times propagate has been called")


#+EAGER
(defvar *not-within-propagate* t
  "Set to nil within propagate")


#+EAGER
(defvar *do-not-eval-list* nil
  "Contains a list of formulas that should not be evaluated during an
  iteration of the constraint solver")


#+EAGER
;;; types of evaluation--normal, in a cycle, or evaluation of a new formula
;;; 
(defvar *eval-type* :normal)



#+EAGER
(defmacro set-cycle-bit (formula value)
  `(setf (a-formula-bits ,formula)
	 (if ,value
	     (logior (a-formula-bits ,formula) ,*cycle-mask*)
	     (logand (a-formula-bits ,formula) ,(lognot *cycle-mask*)))))


#+EAGER
(defmacro set-eval-bit (formula value)
  `(setf (a-formula-bits ,formula)
	 ,(if value
	      `(logior (a-formula-bits ,formula) ,*eval-mask*)
	      `(logand (a-formula-bits ,formula) ,(lognot *eval-mask*)))))



#+EAGER
(defmacro set-visited-bit (formula value)
  `(setf (a-formula-bits ,formula)
	 ,(if value
	      `(logior (a-formula-bits ,formula) ,*visited-mask*)
	      `(logand (a-formula-bits ,formula) ,(lognot *visited-mask*)))))



#+EAGER
(defmacro set-valid-bit (formula value)
  `(if ,value
       (setf (a-formula-valid ,formula) (1- *eval-count*))
       (setf (a-formula-valid ,formula) *eval-count*)))



#+EAGER
(defmacro set-renumber-bit (formula value)
  `(setf (a-formula-bits ,formula)
	 ,(if value
	      `(logior (a-formula-bits ,formula) ,*renumber-mask*)
	      `(logand (a-formula-bits ,formula) ,(lognot *renumber-mask*)))))



#+EAGER
(defmacro set-fixed-bit (formula value)
  `(setf (a-formula-bits ,formula)
	 ,(if value
	      `(logior (a-formula-bits ,formula) ,*fixed-mask*)
	      `(logand (a-formula-bits ,formula) ,(lognot *fixed-mask*)))))



#+EAGER
(defmacro prev-priority (index)
  `(aref *prev-priority-array* ,index))


#+EAGER
(defmacro succ-priority (index)
  `(aref *succ-priority-array* ,index))


#+EAGER
(defmacro priority-value (index)
  `(car (aref *priority-array* ,index)))


#+EAGER
(defmacro priority-<=-p (p1 p2)
  `(<= (priority-value ,p1) (priority-value ,p2)))


#+EAGER
(defmacro priority-<-p (p1 p2)
  `(< (priority-value ,p1) (priority-value ,p2)))


#+EAGER
(defmacro priority-=-p (p1 p2)
  `(= ,p1 ,p2))


#+EAGER
(defmacro priority->-p (p1 p2)
  `(> (priority-value ,p1) (priority-value ,p2)))


#+EAGER
(defmacro priority->=-p (p1 p2)
  `(>= (priority-value ,p1) (priority-value ,p2)))


#+EAGER
(defmacro min-priority (p1 p2)
  `(if (priority-<=-p ,p1 ,p2)
       ,p1
       ,p2))



#+EAGER
(defmacro max-priority (p1 p2)
  `(if (priority->=-p ,p1 ,p2)
       ,p1
       ,p2))



#+EAGER
(defmacro dolist-test-elim ((list-var list test) &body body)
  `(let ((dotest-prev ,list))
     (do ((list-vars ,list list-vars)) ; loop control handled in loop
	 ((null list-vars) ,list)
       (let ((,list-var (car list-vars)))
	 (if ,test
	     (progn
	       ,@body
	       ; update the loop variables
	       (setf dotest-prev list-vars)
	       (setf list-vars (cdr list-vars)))
	     ; if element does not meet test, remove it from the list
	     (if (eq list-vars ,list) ; if front of list
		 (progn
		   (pop list-vars)
		   (setf ,list list-vars)
		   (setf dotest-prev list-vars))
		 (progn
		   (pop (cdr dotest-prev))
		   (setf list-vars (cdr dotest-prev)))))))))



#+EAGER
(defmacro dolist-test ((list-var list test) &body body)
  `(do ((list-vars ,list (cdr list-vars)))
      ((null list-vars))
    (let ((,list-var (car list-vars)))
      (when ,test
	,@body))))




;;;  -------------------------------------------------- Low-level slot access


(defmacro deleted-p (schema)
  `(locally (declare ,*special-kr-optimization*)
     (null (schema-bins ,schema))))


(defmacro not-deleted-p (schema)
  `(locally (declare ,*special-kr-optimization*)
     (schema-bins ,schema)))


(defmacro is-inherited (bits)
  `(logbitp ,*inherited-bit* ,bits))


(defmacro is-parent (bits)
  `(logbitp ,*is-parent-bit* ,bits))


(defmacro is-constant (bits)
  `(logbitp ,*is-constant-bit* ,bits))


(defmacro is-update-slot (bits)
  `(logbitp ,*is-update-slot-bit* ,bits))

(defmacro set-is-update-slot (bits)
  `(logior ,*is-update-slot-mask* ,bits))


(defmacro is-local-only (bits)
  `(logbitp ,*is-local-only-slot-bit* ,bits))


(defmacro is-parameter (bits)
  `(logbitp ,*is-parameter-slot-bit* ,bits))


(defmacro extract-type-code (bits)
  `(logand ,*type-mask* ,bits))

(defmacro get-entry-type-code (entry)
  `(locally (declare ,*special-kr-optimization*)
     (extract-type-code (sl-bits ,entry))))

(defmacro code-to-type (type-code)
  `(svref types-array ,type-code))

(defmacro code-to-type-fn (type-code)
  `(svref type-fns-array ,type-code))

(defmacro code-to-type-doc (type-code)
  `(svref type-docs-array ,type-code))

(defmacro check-kr-type (value code)
  `(funcall (code-to-type-fn ,code) ,value))


;;;; DEF-KR-TYPE
;;;
;;; Create a new type, which can then be used for typechecking.
;;;
(defmacro def-kr-type (typename-or-type &optional args body type-doc)
  "Defines a new type for KR's type-checking mechanism.  You must define
a type using def-kr-type before you can reference that type.  There
are 2 formats for def-kr-type, one named, one un-named, as the following
examples show:

     (def-kr-type my-named-type () '(or keyword null))
     (def-kr-type '(or keyword null))

Note that the first format is the same syntax as Lisp's 'deftype'.
With either definition, you could then specify some object's type to be
(OR KEYWORD NULL).  With the first defn, you could also specify the type
to be \"MY-NAMED-TYPE\".

You can also provide a documentation string as the last parameter, as in:
     (def-kr-type my-named-type () '(or keyword null) \"Sample doc string\")"

  (cond ((listp typename-or-type)
	   (unless (eq (car typename-or-type) 'QUOTE)
	     (error "Illegal typename to def-kr-type: ~S" typename-or-type))
	   (unless (and (null args) (null body) (null type-doc))
	     (error "Illegal specification: (DEF-KR-TYPE ~S ~S ~S ~S)"
			typename-or-type args body type-doc))
	   (setq body typename-or-type)
	   (setq typename-or-type NIL))
        (args
	   (error "DEF-KR-TYPE only works with NULL args, not ~S~%" args))
        (T
	   (setq typename-or-type (symbol-name typename-or-type))))
  (setq body (eval body))
  `(add-new-type ,typename-or-type ',body ,(type-to-fn body) ,type-doc))



;;; -------------------------------------------------- List-or-value code


;;; Member, but with a test of "eq".  Interestingly, if "item" is a keyword,
;;; then it is faster to use the normal member fn!
(defmacro memberq (item list)
  (if (keywordp item)
  `(member ,item ,list)
  `(member ,item ,list :test #'eq)))



;;; Assoc, but with a test of "eq"
(defmacro assocq (item alist)
  (if (keywordp item)
  `(assoc ,item ,alist)
  `(assoc ,item ,alist :test #'eq)))



;;; Execute the <body> on each element of the <list>, or only once if the
;;; <list> is a single value.
;;;
(defmacro do-one-or-list ((var list &optional use-continue) &body body)
  `(let* ((do-one-list ,list)
	  (,var (if (listp do-one-list) (car do-one-list) do-one-list)))
    (block nil
      (tagbody
       again
	 (if (null do-one-list)
	     (return-from nil nil))
	 ,@body
       ,@(if use-continue
	   '(endbody))
	 (if (not (listp do-one-list))
	     (return-from nil nil))
	 (setq do-one-list (cdr do-one-list)
	       ,var (car do-one-list))
	 (go again)))))



(defmacro push-one-or-list (item accessor-form &optional check-new-p)
  `(let ((current ,accessor-form))
    (if (null current)
      (setf ,accessor-form ,item)
      (if (listp current)
	,@(if check-new-p
	    `((if (not (member ,item current))
	      (setf ,accessor-form (cons ,item current))))
	    `((setf ,accessor-form (cons ,item current))))
	,@(if check-new-p
	    `((if (not (eq ,item current))
		(setf ,accessor-form (list ,item current))))
	    `((setf ,accessor-form (list ,item current))))))))



(defmacro delete-one-or-list (item accessor-form)
  `(let ((current ,accessor-form))
    (if (listp current)
      (setf ,accessor-form (delete ,item current))
      (if (eq ,item current)
	(setf ,accessor-form NIL)))))



;;; Allow the current iteration of do-one-or-list to be terminated
;;; prematurely.
;;;
(defmacro continue-out ()
  `(go endbody))



;;; --------------------------------------------------


;;; returns the formula in a dependency
;;; 
(defmacro get-dependent-formula (dependency)
  `(car ,dependency))



(defmacro slot-dependents (slot-structure)
  (let ((entry (gensym)))
    `(locally (declare ,*special-kr-optimization*)
       (let ((,entry ,slot-structure))
      (if (full-sl-p ,entry)
	(full-sl-dependents ,entry))))))


;;; -------------------------------------------------- Low-level slot access


#+GARNET-BINS
(defparameter *bin-indices* (make-array 256)
  "From the first character of a slot name, get the index to the corresponding
  slots bin.")

#+GARNET-BINS
(defparameter *bin-layout* '(	( #\J  #\Y  #\F )
  				( #\K  #\N  #\A  #\B )
  				( #\G  #\M  #\P )
  				( #\O  #\S )
  				( #\V  #\C )
  				( #\X  #\L  #\I )
  				( #\H  #\T  #\D )
  				( #\*  #\E  #\W  #\U  #\R )
			    )
  "After random assignments, letters listed in the *bin-layout* will be
   re-assigned into the appropriate bin.")


;; First assign all letters random bins
#+GARNET-BINS
(dotimes (i 256)
  (setf (svref *bin-indices* i) (mod i *bins-length*)))


;; Then place all the letters in the *bin-layout* where they belong.
#+GARNET-BINS
(when *bin-layout*
  (let ((bin 0))
    (dolist (bin-letters *bin-layout*)
      (dolist (bin-letter bin-letters)
        (setf (svref *bin-indices* (char-code bin-letter)) bin))
      (incf bin))))


#+GARNET-BINS
(defmacro slot-to-bin-index (slot)
  (if (keywordp slot)
    `(svref *bin-indices* ,(char-code (schar (symbol-name slot) 0)))
    `(svref *bin-indices* (char-code (schar (symbol-name ,slot) 0)))))
#|
;;; This version is an optimization for LISPWORKS.
(defmacro slot-to-bin-index (slot)
  `(svref (the (simple-array) *bin-indices*)
    (the fixnum
     ,(if (keywordp slot)
	(char-code (schar (symbol-name slot) 0))
	`(char-code (schar (symbol-name ,slot) 0))))))
|#



;;; RETURNS: a slot structure, or NIL.
;;;
(defmacro slot-accessor (schema slot)
  #+GARNET-BINS
  (let ((entry (gensym)))
    `(locally (declare ,*special-kr-optimization*)
       (dolist (,entry (svref (schema-bins ,schema) (slot-to-bin-index ,slot)))
         (if (eq (sl-name ,entry) ,slot)
	   (return ,entry)))))
  #-GARNET-BINS
  `(values (gethash ,slot (schema-bins ,schema)))
  )



;;; RETURNS: the slot structure it created or modified.
;;;
;;; SIDE EFFECTS: if <dependents> is specified, and the slot structure is
;;; modified to be a full-slot structure.
;;;
(defmacro set-slot-accessor (schema slot value bits the-dependents)
  #+GARNET-BINS
  (let ((the-index (gensym))
	(the-bins (gensym))
	(the-entry (gensym))
	(rest (gensym))
	(dependents (gensym)))
    `(locally (declare ,*special-kr-optimization*)
       (let* ((,the-index (slot-to-bin-index ,slot))
	    (,the-bins (schema-bins ,schema))
	    (,dependents ,the-dependents))
      (unless (do ((,rest (svref ,the-bins ,the-index) (cdr ,rest))
		   ,the-entry)
		  ((null ,rest))
		(setf ,the-entry (car ,rest))
		(when (eq (sl-name ,the-entry) ,slot)
		  (when (and ,dependents (not (full-sl-p ,the-entry)))
		    ;; Need to use a full slot, only have a short one.
		    (setf (car ,rest)
			  (setf ,the-entry (make-full-sl)))
		    (setf (sl-name ,the-entry) ,slot))
		  ;; Slot is present - update it.
		  (setf (sl-value ,the-entry) ,value)
		  (setf (sl-bits ,the-entry) ,bits)
		  (if ,dependents
		    (setf (full-sl-dependents ,the-entry) ,dependents))
		  (return ,the-entry)))
	;; Slot is not present - create it.
	(let ((,the-entry (if ,dependents (make-full-sl) (make-sl))))
	  (setf (sl-name ,the-entry) ,slot)
	  (setf (sl-value ,the-entry) ,value)
	  (setf (sl-bits ,the-entry) ,bits)
	  (if ,dependents
	    (setf (full-sl-dependents ,the-entry) ,dependents))
	  (push ,the-entry (svref ,the-bins ,the-index))
	  ,the-entry)))))
   #-GARNET-BINS
   (let ((the-bins (gensym))
	 (the-entry (gensym))
	 (dependents (gensym)))
     `(let* ((,the-bins (schema-bins ,schema))
	     (,the-entry (gethash ,slot ,the-bins))
	     (,dependents ,the-dependents))
        (if ,the-entry
	    (progn
	      (when (and ,dependents (not (full-sl-p ,the-entry)))
		;; Need to use a full slot, only have a short one.
		(setf (gethash ,slot ,the-bins) (setf ,the-entry (make-full-sl)))
		(setf (sl-name ,the-entry) ,slot))
	      ;; Slot is present - update it.
	      (setf (sl-value ,the-entry) ,value)
	      (setf (sl-bits ,the-entry) ,bits)
	      (if ,dependents
		  (setf (full-sl-dependents ,the-entry) ,dependents))
	      ,the-entry)
	  ;; Slot is not present - create it.
	  (progn
	    (setf ,the-entry (if ,dependents (make-full-sl) (make-sl)))
	    (setf (sl-name ,the-entry) ,slot)
	    (setf (sl-value ,the-entry) ,value)
	    (setf (sl-bits ,the-entry) ,bits)
	    (if ,dependents
		(setf (full-sl-dependents ,the-entry) ,dependents))
	    (setf (gethash ,slot ,the-bins) ,the-entry)))))
   )



;;; --------------------------------------------------


;;; A few specialized accessors for formula slots.
;;;


;;; The "extras" structure slot, which is defined by the <schema> defstruct, is
;;; not used in formulas, so we reuse it to store the formula number.
;;;
(defmacro a-formula-number (formula)
  `(a-formula-bins ,formula))

(defmacro set-formula-number (formula value)
  `(setf (a-formula-bins ,formula) ,value))

(defmacro on-schema (formula)
  `(a-formula-schema ,formula))

(defmacro on-slot (formula)
  `(a-formula-slot ,formula))

(defmacro cached-value (thing)
  `(a-formula-cached-value ,thing))

(defmacro cache-is-valid (thing)
  `(logbitp 0 (a-formula-number ,thing)))


(defmacro set-cache-is-valid (thing value)
  (if value
      `(set-formula-number ,thing (logior (a-formula-number ,thing) 1))
      `(set-formula-number ,thing
	(logand (a-formula-number ,thing) ,(lognot 1)))))


(defmacro cache-mark (thing)
  `(logand (a-formula-number ,thing) ,(lognot 1)))

(defmacro set-cache-mark (thing mark)
  `(set-formula-number ,thing
    (logior (logand (a-formula-number ,thing) 1) ,mark)))



;;; --------------------------------------------------


;;; This is a global because some of KR's internals want to access the
;;; entry on which iterate-slot-value is working.
;;;
(defparameter iterate-slot-value-entry nil
  "Ugly")


;;; Iterate the <body> for all the slots in the <schema>, with the variable
;;; <slot> bound to each slot in turn and the variable <value> bound to
;;; the <slot>'s value.
;;; If <everything> is T, even slots which contain *no-value* (but with same
;;; bit set) are used.
;;; 
(defmacro iterate-slot-value ((a-schema inherited everything check-formula-p)
			      &body body)
  `(locally (declare ,*special-kr-optimization*)
     (,@(if check-formula-p `(if (not (formula-p ,a-schema))) '(progn))
      #+GARNET-BINS
      ;; Process all bins and all slots within.
      (let ((bins (schema-bins ,a-schema)))
       (dotimes (i *bins-length*)
        (dolist (iterate-slot-value-entry (aref bins i))
         (let ((slot (sl-name iterate-slot-value-entry)) ; name for the slot
		                                         (value (sl-value iterate-slot-value-entry)))
          ;; This slot exists
          ,@(if inherited
             ;; Either local or inherited will do.
             (if everything
              ;; Execute on a no-value, too.
              body
              ;; Only execute on real values.
              `((unless (eq value *no-value*)
                 ,@body)))
             ;; Make sure that the slot is not inherited.
             `((unless (is-inherited (sl-bits iterate-slot-value-entry))
                ,@(if everything
                   body
                   `((unless (eq value *no-value*)
                      ,@body))))))))))
      #-GARNET-BINS
     (maphash
      #'(lambda (iterate-ignored-slot-name iterate-slot-value-entry)
 	  (declare (ignore iterate-ignored-slot-name))
 	  (let ((slot (sl-name iterate-slot-value-entry)) ; name for the slot
 		(value (sl-value iterate-slot-value-entry)))
 	    ;; This slot exists
 	    ,@(if inherited
		  ;; Either local or inherited will do.
		  (if everything
		      ;; Execute on a no-value, too.
		      body
		    ;; Only execute on real values.
		    `((unless (eq value *no-value*)
			,@body)))
 		;; Make sure that the slot is not inherited.
 		`((unless (is-inherited (sl-bits iterate-slot-value-entry))
 		    ,@(if everything
			  body
 			`((unless (eq value *no-value*)
 			    ,@body))))))))
      (schema-bins ,a-schema))
     )))
#|
(defmacro iterate-slot-value ((a-schema inherited everything check-formula-p)
			      &body body)
  `(locally (declare ,*special-kr-optimization*)
     (,@(if check-formula-p `(if (not (formula-p ,a-schema))) '(progn))
      (print ,a-schema))))
|#




;;;; DOSLOTS
;;;
;;; Executes the <body> with <slot> bound in turn to each slot in the <schema>.
;;; 
(defmacro doslots ((slot-var a-schema &optional inherited) &body body)
  `(iterate-slot-value (,a-schema ,inherited NIL NIL)
     (let ((,slot-var slot))
       ,@body)))



;;;; GET-LOCAL-VALUE
;;; 
(defun get-local-value (schema slot)
  (locally (declare (optimize (speed 3) (space 0) #+(or ALLEGRO APPLE) (debug 0)))
    (let ((entry (slot-accessor schema slot)))
    (if (if entry (not (is-inherited (sl-bits entry))))
      (sl-value entry)))))



;;; Compatibility only!
;;; 
(defmacro get-local-values (schema slot)
  `(get-local-value ,schema ,slot))



;;; This macro is used by macros such as GV or G-VALUE, which can
;;; be called with any number of slot names and expand into
;;; a nested chain of calls to <accessor-function>.
;;; 
(defmacro expand-accessor (accessor-function schema &rest slots)
  (if slots
      ;; At least one slot was specified.
      (let ((kernel schema))
	;; "Grow" the kernel by wrapping more gv-fn's around it
	(do ((slot slots (cdr slot)))
	    ((null slot))
	  (setf kernel
		`(,accessor-function ,kernel ,(car slot))))
	kernel)
      ;; No slots!
      (error "expand-accessor: at least one slot is required")))



;;;; WITH-CONSTANTS-DISABLED
;;; 
;;; Execute the <body> with constant processing disabled.
;;; 
(defmacro with-constants-disabled (&body body)
  `(let ((*constants-disabled* t))
     ,@body))



;;;; WITH-TYPES-DISABLED
;;; 
;;; Execute the <body> with type declaration processing disabled.
;;; 
(defmacro with-types-disabled (&body body)
  `(let ((*types-enabled* nil))
     ,@body))


;;;; WITH-DEPENDENCIES-DISABLED
;;;
(defmacro with-dependencies-disabled (&body body)
  `(let ((*setup-dependencies* nil))
     ,@body))


;;;; WITH-DEMONS-DISABLED
;;; 
;;; Execute the <body> with pre- and post-demons disabled.
;;; 
(defmacro with-demons-disabled (&body body)
  `(let ((*demons-disabled* t))
     ,@body))



;;;; WITH-DEMON-DISABLED
;;; 
;;; Execute the <body> with a specific demon disabled.
;;; 
(defmacro with-demon-disabled (demon &body body)
  `(let ((*demons-disabled* (disable-a-demon ,demon)))
    ,@body))




;;;; WITH-DEMON-ENABLED
;;; 
;;; Execute the <body> with a specific demon disabled.
;;; 
(defmacro with-demon-enabled (demon &body body)
  `(let ((*demons-disabled* (enable-a-demon ,demon)))
    ,@body))



;;;; RELATION-P
;;; 
(defmacro relation-p (slot)
  `(assocq ,slot *relations*))



;;; This implements g-value, g-local-value, get-value, and get-local-value.
;;; If <inherit-p> is true, generates code to inherit a value; otherwise,
;;; generates code for the local-only case.
;;; If <formula-p> is true, generates code to evaluate formulas; otherwise,
;;; the formula object itself is returned.
;;;
(defmacro g-value-body (schema slot inherit-p formula-p)
  (let ((schema-form (if (symbolp schema) schema 'schema))
	(entry (gensym))
	(value (gensym)))
    `(locally (declare ,*special-kr-optimization*)
       (let* (,@(unless (symbolp schema) `((schema ,schema)))
	    (,entry
	     #+GARNET-DEBUG
	     (if (is-schema ,schema-form) ; this is just schema-p
	       ;; make sure it's not a formula or deleted
	       (let ((bins (schema-bins ,schema-form)))
		 (if (and bins (not (integerp bins)))
		   (slot-accessor ,schema-form ,slot)
		   (error "Non-object ~S in g-value or get-value (slot is ~S)"
			  ,schema-form ,slot)))
	       (error "Non-object ~S in g-value or get-value (slot is ~S)"
		      ,schema-form ,slot))
	     #-GARNET-DEBUG
	     (slot-accessor ,schema-form ,slot))
	    (,value (if ,entry
		      ,@(if (not inherit-p)
			  `((if (is-inherited (sl-bits ,entry))
			      ,@(if formula-p
				  `((if (a-formula-p (sl-value ,entry))
				      (sl-value ,entry)))
				  `(NIL))
			      (sl-value ,entry)))
			  `((sl-value ,entry)))
		      ,@(if (or inherit-p formula-p)
			  `(*no-value*)))))
      (if (eq ,value *no-value*)
	,@(cond ((and (not inherit-p) (not formula-p))
		 `((setf ,value NIL)))
		((and (not inherit-p) formula-p)
		 `((if ,entry
		     (setf ,value NIL)
		     (if (not (formula-p (setf ,value
					       (g-value-inherit-values
						,schema-form ,slot T NIL))))
		       (setf ,value NIL)))))

		((a-local-only-slot slot)
		 ;; slots such as :IS-A-INV should never be inherited!
		 `((setf ,value NIL)))
		(t
		 `((if (if ,entry (is-inherited (sl-bits ,entry)))
		     ;; in which case, no-value was already inherited.
		     (setf ,value NIL)
		     ;; otherwise, try to inherit the value.
		     (progn
		       (setf ,value (g-value-inherit-values ,schema-form ,slot
							    T ,entry))
		       (if (eq ,value *no-value*)
			 (setf ,value NIL))))))))
      ,@(if formula-p
	  `((if (a-formula-p ,value)
	      (g-value-formula-value ,schema-form ,slot ,value ,entry)
	      ,value))
	  `(,value))))))



;;;; GET-VALUE
;;; 
(defmacro get-value (schema slot)
  `(g-value-body ,schema ,slot T NIL))



#|
;;; GET-VALUES
;;; 
(defmacro get-values (schema slot)
  `(let ((values (get-value ,schema ,slot)))
     (if (listp values)
	 values
	 (list values))))
|#


;;;; G-VALUE
;;; This macro expands into nested calls to g-value-fn.  For example:
;;; (g-value schema :slot1 :slot2 :slot3 5) expands into
;;; (g-value-fn (g-value-fn (g-value-fn schema :slot1 0) :slot2 0) :slot3 5)
;;; 
(defmacro g-value (schema &rest slots)
  (if slots
      `(expand-accessor value-fn ,schema ,@slots)
    `(progn ,schema)))



;;;; G-LOCAL-VALUE
;;;
(defmacro g-local-value (schema &rest slots)
  (if slots
      `(expand-accessor g-local-value-fn ,schema ,@slots)
      `(progn ,schema)))



;;; Used to look in the :UPDATE-SLOTS of the <schema> to determine whether the
;;; <slot> has an associated demon.  This gives us the freedom to let different
;;; schemata have demons on possibly different slots.
;;; 
;;; Now, it uses the <slot>'s is-update-slot bit to check.  This bit is set at
;;; create-instance time by traversing the :UPDATE-SLOTS list of the <schema>.
;;; 
(defmacro slot-requires-demon (schema slot &optional entry)
  `(locally (declare ,*special-kr-optimization*)
     (let ((.entry. (or ,entry (slot-accessor ,schema ,slot))))
    (if .entry.
        (is-update-slot (sl-bits .entry.))))))

#+COMMENT
(defmacro slot-requires-demon (schema slot &optional entry)
  `(let ((update (get-value ,schema, :UPDATE-SLOTS)))
    (or (eq (car update) T)
     (memberq ,slot update))))



;;; Execute the update demon associated with the <schema> and <slot>, if there
;;; is one.
;;; 
(defmacro run-invalidate-demons (schema slot entry)
  `(unless (eq *demons-disabled* T)
    (if (slot-requires-demon ,schema ,slot ,entry)
      (let ((demon (get-value ,schema :INVALIDATE-DEMON)))
        (if demon
          (unless (demon-is-disabled demon)
	    (funcall demon ,schema ,slot nil)))))))



;;; Invokes the pre-set demon, if one is defined and if the <slot> is an
;;; "interesting" slot (i.e., if it is listed in the :update-slots of the
;;; <schema>).
;;; Also, if *slot-setter-debug* is bound, it invokes it.  This is a debugging
;;; function that gets called every time a slot is modified, either by s-value
;;; or as a result of formula evaluation.  The <reason> is given as the fourth
;;; parameter to the function; it is a keyword that explains why the slot
;;; was changed.
;;;
(defmacro run-pre-set-demons (schema slot new-value is-formula reason)
  #-GARNET-DEBUG
  (declare (ignore reason))
  `(unless (eq *demons-disabled* T)
    #+GARNET-DEBUG
    (if *slot-setter-debug*
	(funcall *slot-setter-debug* ,schema ,slot ,new-value ,reason))
    (if *pre-set-demon*
      (if (not (demon-is-disabled *pre-set-demon*))
	(if (slot-requires-demon ,schema ,slot)
	  (if ,@(if is-formula
		  `((not (equal
			  ,new-value
			  ,@(cond ((eq is-formula :CURRENT-FORMULA)
				   `((cached-value *current-formula*)))
				  ((eq is-formula T)
				   `((g-cached-value ,schema ,slot)))
				  (t
				   `(,is-formula))))))
		  `(T))
	      (funcall *pre-set-demon* ,schema ,slot ,new-value)))))))



;;; Helper function for multi-level S-VALUE
;;;
(defun s-value-chain (schema &rest slots)
  (locally (declare (optimize (speed 3) (space 0) #+(or ALLEGRO APPLE) (debug 0)))
  (if (null schema)
    (error "S-VALUE on a null object:  (S-VALUE ~S~{ ~S~})" schema slots)
    (unless (schema-p schema)
      (error "S-VALUE called with the non-object ~S :  (s-value ~S~{ ~S~})."
	     schema schema slots)))
  (do* ((s slots (cdr s))
	(intermediate schema))
       ((null (cddr s))
	(s-value-fn intermediate (first s) (second s)))
    (let ((new-schema (value-fn intermediate (car s))))
      (if (null new-schema)
	(error
	 "An intermediate schema is null:  slot ~S of object ~S has value
  NIL in (S-VALUE ~S~{ ~S~})"
	       (car s) intermediate schema slots)
	(unless (schema-p new-schema)
	  (error "An intermediate value is not a schema in (S-VALUE ~S~{ ~S~}),
at slot ~S  (non-schema value is ~S, last schema was ~S)"
		 schema slots (car s) new-schema intermediate)))
      (setf intermediate new-schema)))))



;;;; S-VALUE
;;; The basic value-setting macro.
;;; 
;;; Inputs:
;;; - <schema>: the name of a schema
;;; - <slot>: name of the slot to be modified.
;;; - <value>: new value for the <slot>.
;;; 
(defmacro s-value (schema &rest slots)
  (if slots
    ;; This is the more general case.
    (if (cddr slots)
      ;; Several slots.
      `(s-value-chain ,schema ,@slots)
      ;; One (non-special) slot only.
      `(s-value-fn ,schema ,(first slots) ,(second slots)))))



;;;; DOVALUES
;;; Executes <body> with <variable> bound to all the values of the <slot> in
;;; <schema>.
;;; 
(defmacro dovalues ((variable schema slot &key (local nil) (result nil)
			      (formulas T) (in-formula NIL))
		    &rest body)
  `(locally (declare ,*special-kr-optimization*)
     (let* ((schema ,@(if (eq schema :SELF)
			`(*schema-self*)
			`(,schema)))
	  (values ,@(if local
		      (if formulas
			`((g-local-value schema ,slot))
			`((get-local-value schema ,slot)))
		      (if formulas
			(if in-formula
			    `((gv schema ,slot))
			    `((g-value schema ,slot)))
			(if in-formula
			  `((gv schema ,slot))
			  `((get-value schema ,slot)))))))
     ;; Now iterate
     (if values
       (progn
	 (unless (listp values)
	   (format t "(DOVALUES ~s ~s) does not contain a list of values!~%"
		   ,schema ,slot)
	   (setf values (list values)))
	 ;; Extra code for the case FORMULAS = T
	 (dolist (,variable values)
	   ,@(if formulas
	       ;; Generate test for formula-p, unless :FORMULAS is nil
	       `((when (formula-p ,variable)
		       #+EAGER
		       (propagate)
		       (setf ,variable
			     #+EAGER
			     (cached-value ,variable)
			     #-EAGER
			     (g-value-formula-value
			      schema ,slot ,variable NIL)))))
	   ,@body)))
     ,result)))





;;;; CREATE-RELATION
;;;
;;; Defines a new relation with its inverses.  In <inheritance-p> is non-nil,
;;; classifies the relation as one that performs inheritance.
;;; Note that <relation> should be a slot name, not a schema.
;;; 
(defmacro create-relation (relation inheritance-p &rest inverses)
  (let ((entry (gensym)))
    `(let ((inverses ',inverses))
      (when ,inheritance-p
	(pushnew ,relation *inheritance-relations*)
	(dolist (inverse inverses)
	  (pushnew inverse *inheritance-inverse-relations*)))
      (unless (assocq ,relation *relations*)
	(push (cons ,relation inverses) *relations*))
      (dolist (inv inverses)
	(let ((,entry (assocq inv *relations*)))
	  (if ,entry
	    (pushnew ,relation (cdr ,entry))
	    (progn
	      (push (list inv ,relation) *relations*))))))))



;;;; HAS-SLOT-P
;;; 
(defun has-slot-p (schema slot)
  (locally (declare (optimize (speed 3) (space 0) #+(or ALLEGRO APPLE) (debug 0)))
    (let ((entry (slot-accessor schema slot)))
    (if entry
      (if (not (eq (sl-value entry) *no-value*))
	(not (is-inherited (sl-bits entry))))))))



;;;; SET-VALUES
;;;
;;; This is here for compatibility purposes.
;;;
(defmacro set-values (schema slot values)
  `(if (relation-p ,slot)
       (s-value ,schema ,slot (if (listp ,values) ,values (list ,values)))
       (s-value ,schema ,slot ,values)))




;;;; KR-SEND
;;; 
;;; 
(defmacro kr-send (schema slot &rest args)
  (let ((the-schema (gensym))
	(the-function (gensym)))
    `(let* ((,the-schema ,schema)
	    (,the-function (g-value ,the-schema ,slot)))
      (if ,the-function
	  ;; Bind these in case call prototype method is used.
	  (let ((*kr-send-self* ,the-schema)
		(*kr-send-slot* ,slot)
		(*kr-send-parent* NIL))
	    (funcall ,the-function ,@args))))))



;;;; CALL-PROTOTYPE-METHOD
;;; 
(defmacro call-prototype-method (&rest args)
  (let ((entry (gensym)))
    `(locally (declare ,*special-kr-optimization*)
       (let ((first-c-p-m (and (null *kr-send-parent*)
			     (let ((,entry (slot-accessor *kr-send-self*
							  *kr-send-slot*)))
			       (or (null ,entry)
				   (is-inherited (sl-bits ,entry)))))))
      (multiple-value-bind (method new-parent)
	  (find-parent *kr-send-self* *kr-send-slot*)
	(when method
	  (if first-c-p-m
	    (multiple-value-setq (method *kr-send-parent*)
	      (find-parent new-parent *kr-send-slot*))
	    (setf *kr-send-parent* new-parent))
	  (if method
	    (let ((*kr-send-self* *kr-send-parent*))
	      (funcall method ,@args)))))))))



;;;; APPLY-PROTOTYPE-METHOD
;;;
(defmacro apply-prototype-method (&rest args)
  (let ((entry (gensym)))
    `(locally (declare ,*special-kr-optimization*)
       (let ((first-c-p-m (and (null *kr-send-parent*)
			     (let ((,entry (slot-accessor *kr-send-self*
							  *kr-send-slot*)))
			       (or (null ,entry)
				   (is-inherited (sl-bits ,entry)))))))
      (multiple-value-bind (method new-parent)
	  (find-parent *kr-send-self* *kr-send-slot*)
	(when method
	  (if first-c-p-m
	    (multiple-value-setq (method *kr-send-parent*)
	      (find-parent new-parent *kr-send-slot*))
	    (setf *kr-send-parent* new-parent))
	  (if method
	    (let ((*kr-send-self* *kr-send-parent*))
	      (apply method ,@args)))))))))



;;;; DEFINE-METHOD
;;; 
(defmacro define-method (name class arg-list &rest body)
  (unless (keywordp name)
    (setf name (intern (symbol-name name) (find-package "KEYWORD")))
    (format t "DEFINE-METHOD takes a keyword as the method name - using ~S~%"
	    name))
  (let* ((function-name (intern (concatenate 'string
					     (symbol-name name)
					     "-METHOD-"
					     (symbol-name class)))))
    `(progn
       (defun ,function-name ,arg-list
	 ,@body)
       (s-value ,class ,name ',function-name))))



;;;; METHOD-TRACE
;;; 
(defmacro method-trace (class generic-fn)
  `(let ((fn (g-value ,class ,generic-fn)))
    (if fn
      (eval `(trace ,fn)))))



;;;; CREATE-SCHEMA
;;; 
;;; The keyword :OVERRIDE may be used to indicate that the schema should
;;; be kept, if it exists, and newly specified slots should simply override
;;; existing ones.  The default behavior is to wipe out the old schema.
;;; 
(defmacro create-schema (name &rest rest)
  (let ((prefix (memberq :NAME-PREFIX rest)))
    ;; Check that all elements of the list are well-formed, give warnings
    ;; otherwise
    (when (and prefix (null name))
      ;; We have an unnamed schema but a name prefix - use it.
      (setf name (second prefix))
      (setf prefix NIL))
    (when prefix
      (format
       t "Warning - you specified both a name and a :NAME-PREFIX option~:
       in (create-schema ~S).~%   Ignoring the :NAME-PREFIX.~%"
       name)
      (setf prefix nil))
    ;; Make the schema name known at compile time, so we do not issue
    ;; silly warnings.
    (if (and (listp name) (eq (car name) 'QUOTE))
      (proclaim `(special ,(eval name))))
    (let* ((override (not (null (memberq :OVERRIDE rest))))
	   (destroy (and name		;; avoid trouble with (c-s NIL :override)
			 (not override)))
	   (*create-schema-schema* name)
	   (slots (process-slots rest))
	   (generate-instance (not (null (memberq :generate-instance rest)))))
      (creation-message name)
      `(do-schema-body
	,(if destroy
	   `(make-a-new-schema ,name)
	   (if (and (listp name)
		    (eq (car name) 'QUOTE)
		    (boundp (second name)))
	     (eval name)
	     `(make-a-new-schema ,name)))
	,(car slots)			; is-a
	,generate-instance ; create instance
	,(null (memberq :delayed-processing rest)) ; process constant slots
	,override
	,@(cdr slots)))))		; types, plus slot specifiers



;;; --------------------------------------------------


;;;; CREATE-PROTOTYPE
;;; 
(defmacro create-prototype (name &rest slots)
  `(create-schema ,name ,@slots))



;;;; CREATE-INSTANCE
;;; 
(defmacro create-instance (name class &body body)
  (when (and (listp class)
	     (eq (car class) 'QUOTE))
    ;; Prevent a common mistake.
    (cerror
     "Remove the quote and use the resulting object."
     "  Quoted symbols cannot be used as prototypes: (create-instance ~S ~S)~%"
     name class)
    (setf class (eval (second class))))
  ;; Everything is OK.
  (dolist (element body)
    (when (and (listp element) (eq (car element) :IS-A))
      (format
       t
       "CREATE-INSTANCE ~S ~S: do not specify the :IS-A slot!  Ignored.~%"
       name class)
      (setf body (remove (assocq :IS-A body) body))))
  `(create-schema ,name :GENERATE-INSTANCE
    ;; class might be nil, which means no IS-A slot
    ,@(if class `((:is-a ,class)))
    ,@body))



;;; BEGIN-CREATE-INSTANCE
;;;
;;; Processes the first half of a create-instance where constant-slot
;;; processing needs to be delayed.
;;; This should only be used for specialized applications, such as those
;;; found in aggrelists.
;;;
(defmacro begin-create-instance (name class &body body)
  (dolist (descriptor body)
    (when (and (listp descriptor) (eq (car descriptor) :IS-A))
      (format
       t
       "BEGIN-CREATE-INSTANCE ~S ~S: do not specify the :IS-A slot!  Ignored.~%"
       name class)
      (setf body (remove descriptor body))
      (return)))
  `(create-schema ,name :DELAYED-PROCESSING
     ;; class might be nil, which means no IS-A slot
     ,@(if class `((:is-a ,class)))
     ,@body))




;;; ---------------------------------------- Setf forms for several macros


(defsetf g-value s-value)

(defsetf get-values s-value)

(defsetf get-local-values s-value)

(defsetf g-local-value s-value)

;;; At the top-level, (setf (gv ...)) behaves just like s-value; when
;;; inside a formula, it also sets up a dependency, just like gv would.
;;;
(defsetf gv (schema &rest slots) (value)
  `(progn
    (if *current-formula*
      (gv ,schema ,@slots))
    (s-value ,schema ,@slots ,value)))



;;; --------------------------------------------------

;;; Internal debugging function
;;; 
(defmacro with (schema slot &body form)
  `(let* ((*schema-self* (if (numberp ,schema) (s ,schema) ,schema))
	  (*schema-slot* ,slot)
	  (*current-formula* (get-value *schema-self* *schema-slot*))
	  (*warning-on-null-link* T))
     (catch 'no-link
       ,@form)))
