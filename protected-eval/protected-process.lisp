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


;;; RGA 11/30/94 --- This puts the protected eval in at a low level in
;;; the garnet code so it should be able to catch any error resulting
;;; from computations intiating in the UI.

(in-package :opal)

;; RGA --- Use excl:*initial-termial-io* as default stream.
#+allegro
(defun launch-main-event-loop-process (&optional (tty excl:*initial-terminal-io*))
  "Spawn a process which is doing Garnet interaction all of the time.
   RETURN the process."
   (when (eq (type-of *main-event-loop-process*) 'mp:process)
      (mp:process-kill *main-event-loop-process*))
   (setf *main-event-loop-process*
          (mp:process-run-restartable-function
	    #+(or allegro-v4.0 allegro-v4.1)
 	    `(:name "Garnet event loop"
              :initial-bindings
              ,(acons '*terminal-io* tty
                       excl:*cl-default-special-bindings*))
	    #-(or allegro-v4.0 allegro-v4.1)
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
		;; RGA added an abort restart to the main event loop.
		(let ((root-window (gv device-info :current-root)))
		  (restart-case
		      (gg:with-garnet-error-handling
			  "Main Interaction Loop"
			(loop
			  (inter::default-event-handler root-window)))
		    (abort () :report "Discard pending X events, restart loop"
			(discard-all-pending-events)))))
	    tty))
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
		    (gg:with-garnet-error-handling
		     "Main Interaction Loop"
		     (loop
		      (inter::default-event-handler root-window)))
		    (abort () :report "Discard pending X events, restart loop"
			   (discard-all-pending-events))))))
	    :name "Garnet event loop"))
   (setf mp::*idle-process* mp::*initial-process*)
   ;; Lower the timeout for serve-event to give good process response.
   (setf lisp::*max-event-to-usec* 50000)
   (setf lisp::*max-event-to-sec* 0)
   *main-event-loop-process*)


(when user::launch-process-p
  (when *main-event-loop-process*
    (kill-main-event-loop-process))
  (launch-main-event-loop-process))

(setf (get :garnet-modules :protected-process) t)
