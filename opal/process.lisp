;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	Begin Change Log
;;;-------------------------------------------------------------------------
;;;
;;;     Gilham  20-Aug-98 Fixed problem with CMUCL process event handling.
;;;     Crosher      1998 Added CMUCL process support.
;;;     Mickish  4-Dec-93 Removed erroneous defvar of opal::update-locking-p
;;;     Almond/Mickish  18-Oct-93  Replaced lexical closure around launch-main-
;;;                                event-loop with default value of TTY.
;;;     Almond/Mickish  17-Sep-93  Added lexical closure around launch-main-
;;;                                event-loop, added restart-case
;;;     Clive Tong  06-Sep-93 Fixed lispworks stuff
;;;     Mickish  6-Aug-93 Added lispworks stuff
;;;     Almond   5-Jan-92 Bound *trace-output* in launch-mel-process
;;;     Dzg/Mickish 21-Sep-92 Added Update-Start-Fn and Update-Stop-Fn
;;;     Myers   20-Aug-92 Added running-main-event-loop-process-elsewhere-p
;;;     Almond  26-May-92 Added patch to launch-main-event-loop-process
;;;			  to handle background streams for Lapidary.
;;;     Pervin  21-Apr-92 Added main-event-loop-process-running-p
;;;     Pervin  14-Apr-92 Uncommented out process code.
;;;			  Got it to work on HP.
;;;     Pervin  30-Mar-92 Commented out process code.
;;;     Pervin  25-Mar-92 Made to be permanent part of Opal.
;;;			  Merged process-allegro and process-lucid.   
;;;     Pervin   9-Aug-90 Released for Garnet.
;;;	Stork	18-Jul-90 Created.
;;;
;;;-------------------------------------------------------------------------
;;;	End Change Log
;;;
;;;
(in-package "OPAL")


;;;===========================================================================
;;;
;;;  Global variables
;;;
;;;===========================================================================

(defparameter *main-event-loop-process* nil
  "The variable which is a handle to the main-event-loop process.")

(defparameter *update-lock*
  #+ALLEGRO (mp:make-process-lock :name "UPDATE-LOCK")
  #+lispworks (mp:make-lock)
  #+(and cmu mp) (mp:make-lock "UPDATE-LOCK")
  #-(or ALLEGRO lispworks (and cmu mp)) NIL)


;;;===========================================================================
;;;
;;;  Define opal:launch-main-event-loop-process
;;;
;;;===========================================================================

(defun discard-all-pending-events ()
  (gem:discard-pending-events (g-value device-info :current-root)))


#+allegro
   ;; RGA --- The optional tty parameter ensures that processes will not
   ;; clobber each other's I/O.
(defun launch-main-event-loop-process (&optional (tty excl:*initial-terminal-io*))
  "Spawn a process which is doing Garnet interaction all of the time.
   RETURN the process."
  (when (eq (type-of *main-event-loop-process*) 'mp:process)
    (mp:process-kill *main-event-loop-process*))
  (setf *main-event-loop-process*
	(mp:process-run-restartable-function
	 #+(or allegro-v4.0 allegro-v4.1 allegro-v4.2)
	 `(:name "Garnet event loop"
	   :initial-bindings
	   ,(acons '*terminal-io* tty
		   excl:*cl-default-special-bindings*))
	 #-(or allegro-v4.0 allegro-v4.1 allegro-v4.2)
	 "Garnet event loop"
	 #'(lambda (t-io)
	     ;; RGA --- This gets around a "feature" of Allegro 4.1
	     ;; which does not allow background processes to do io.
	     ;; Note that the main process function is now a function
	     ;; of one arg which is bound to *terminal-io*
	     (setq *terminal-io* t-io)
	     (setq *query-io* t-io)
	     (setq *standard-input* t-io)
	     (setq *standard-output* t-io)
	     (setq *error-output* t-io)
	     (setq *trace-output* t-io)
	     ;; Don't bind *debug-io* because RGA suggests other problems
	     ;; might arise
					;(setq *debug-io* t-io)
	     ;; first, throw away any pending events
	     (discard-all-pending-events)

	     #-NO-K-READER
	     (eval-when (eval compile load)
	       (set-dispatch-macro-character #\# #\k (function kr::k-reader)))

	     ;; RGA added an abort restart to the main event loop.
	     (let ((root-window (gv device-info :current-root)))
	       (restart-case
		(loop
		 (inter::default-event-handler root-window))
		(abort () :report "Discard pending X events, restart loop"
		       (discard-all-pending-events)))))
	 tty))
  (setf (mp:process-priority *main-event-loop-process*) 1)
  *main-event-loop-process*)


#+lispworks
(defun launch-main-event-loop-process ()
  "Spawn a process which is doing Garnet interaction all of the time.
   RETURN the process."
   (when (eq (type-of *main-event-loop-process*) 'mp:process)
      (mp:process-kill *main-event-loop-process*))
   (setf *main-event-loop-process*
          (mp:process-run-function
	    "Garnet event loop"
 	    `(:name "Garnet event loop")
            #'(lambda (t-io)
		(declare (ignore t-io))
		;; first, throw away any pending events
		(discard-all-pending-events)
		(let ((root-window (gv device-info :current-root)))
		  (restart-case
		   (loop
		    (inter::default-event-handler root-window))
		   (abort () :report "Discard pending X events, restart loop"
			  (discard-all-pending-events)))))
            *terminal-io*))
   (setf (mp:process-priority *main-event-loop-process*) 1)
   *main-event-loop-process*)


#+(and cmu mp)
(defun launch-main-event-loop-process ()
  "Spawn a process which is doing Garnet interaction all of the time.
   RETURN the process."
   (when (mp:processp *main-event-loop-process*)
      (mp:destroy-process *main-event-loop-process*))
   (setf *main-event-loop-process*
	 (mp:make-process
            #'(lambda ()
		;; first, throw away any pending events
		(discard-all-pending-events)
		(let ((root-window (gv device-info :current-root)))
		  (loop
		   (restart-case
		    (loop
		     (inter::default-event-handler root-window))
		    (abort () :report "Discard pending X events, restart loop"
			   (discard-all-pending-events))))))
	    :name "Garnet event loop"))
   (setf mp::*idle-process* mp::*initial-process*)
   ;; Lower the timeout for serve-event to give good process response.
   (setf lisp::*max-event-to-usec* 50000)
   (setf lisp::*max-event-to-sec* 0)
   *main-event-loop-process*)


#-(or allegro lispworks (and cmu mp))
(defun launch-main-event-loop-process ())


;;;===========================================================================
;;;
;;;  Define opal:kill-main-event-loop-process
;;;
;;;===========================================================================

#+(or allegro lispworks)
(defun kill-main-event-loop-process ()
  "
  Kill the current main-event-loop process.
  "
  (when (eq (type-of *main-event-loop-process*) 'mp:process)
    (mp:process-kill *main-event-loop-process*)
    (setf *main-event-loop-process* nil)))


#+(and cmu mp)
(defun kill-main-event-loop-process ()
  "
  Kill the current main-event-loop process.
  "
  (when (and *main-event-loop-process*
             (mp:processp *main-event-loop-process*))
    (mp:destroy-process *main-event-loop-process*)
    (setf *main-event-loop-process* nil)))

#-(or allegro lispworks (and cmu mp))
(defun kill-main-event-loop-process ())

;;;===========================================================================
;;;
;;;  Define running-p functions
;;;
;;;===========================================================================

(defun main-event-loop-process-running-p ()
  (and opal::*main-event-loop-process*
       ;;; Franz's comments about mp:process-runnable-p:  It is true of any
       ;;; process that has a stack-group (meaning that is has been reset and
       ;;; has not yet exhausted its computation), has at least one run reason,
       ;;; has zero arrest reasons, and is not blocked in a call like
       ;;; PROCESS-WAIT or any of its close relatives.  This last clause --
       ;;; testing that the process is not blocked in PROCESS-WAIT --
       ;;; perhaps isn't what you want.  If the process happens temporarily
       ;;; to be waiting for something, it won't be killed.  Perhaps you
       ;;; want to use the PROCESS-ACTIVE-P predicate instead, which
       ;;; is true whether or not the process is in a PROCESS-WAIT.
       #+allegro
       (not (mp:process-runnable-p
		opal::*main-event-loop-process*))
       #+old-lispworks
       (not (mp::process-active-p
	        opal::*main-event-loop-process*))
       #+lispworks
       ;; CT 2Sep93
       (not (and
	     (not (mp::process-wait-reason opal::*main-event-loop-process*))
	     (mp::process-run-reasons opal::*main-event-loop-process*)
	     (not (mp::process-arrest-reasons opal::*main-event-loop-process*))
	     ))
       #+(and cmu mp)
       (not (equal "Run"
		   (mp:process-whostate opal::*main-event-loop-process*)))
       ))

(defun running-main-event-loop-process-elsewhere-p ()
  (and opal::*main-event-loop-process*
       (not (eq opal::*main-event-loop-process*
		#+(or allegro lispworks (and cmu mp)) mp:*current-process*
		#-(or allegro lispworks (and cmu mp)) T)
	    )))


;;;===========================================================================
;;;
;;;  Define process lock functions
;;;
;;;===========================================================================

(defun update-start-fn (window)
  (declare (ignore window))
  #+ALLEGRO
  (if user::update-locking-p
      (unless (eq (mp:process-lock-locker *update-lock*) mp:*current-process*)
	;; Lock only if lock is held by a different process, or unlocked.
	(mp:process-lock *update-lock*)))
  #+old-lispworks
  (if user::update-locking-p
      (mp:process-lock *update-lock*))

  #+lispworks
  ;; CT 2Sep93
  (if user::update-locking-p
      (unless (eq (mp:lock-owner *update-lock*) mp:*current-process*)
	;; Lock only if lock is held by a different process, or unlocked.
	(mp:process-lock *update-lock*))))


(defun update-stop-fn (window)
  (declare (ignore window))
  #+ALLEGRO
  (if (and user::update-locking-p
	   (eq (mp:process-lock-locker *update-lock*) mp:*current-process*))
      (mp:process-unlock *update-lock*))
  #+old-lispworks
  (if user::update-locking-p
      (mp:process-unlock *update-lock*))
  
  #+lispworks
  ;; CT 3Sep93
  (if (and user::update-locking-p
	   (eq (mp:lock-owner *update-lock*) mp:*current-process*))
      (mp:process-unlock *update-lock*)))
