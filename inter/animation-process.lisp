;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
============================================================
Change log:
   10/1/93 Brad Myers - Workaround bug in HP CL where vector-push-extend
                        returns the wrong value
   9/06/93 Clive Tong - Added LispWorks stuff
  10/22/92 Brad Myers - Test if main-event-loop crashed into debugger
  10/11/92 Brad Myers - declare (ignore) so fewer warnings in CMU CL -Sjolin
  5/22/92 Brad Myers - started, based on code from R J Williams
                          <rodw@cbl.leeds.ac.uk> and code from
                          opal:process.lisp
============================================================
|#

(in-package "INTERACTORS")

;;; A window waiting for a timer event

;;; A array of all interactors that have timers.  None are ever removed
;;; from this array.  An index into this array is sent with the timer
;;; event so that we will know which interactor to wake up.
(defparameter *Inters-With-Timers*
  (make-array 5 :fill-pointer 0 :adjustable T))

;; set by default-event-handler.
(defparameter *Process-With-Main-Event-Loop* NIL) 


(defparameter *All-Timer-Processes* NIL)

(defun Reset-All-Timer-Processes ()
  (dolist (p (copy-list *All-Timer-Processes*))
    (internal-kill-timer-process p)))

;; Returns T if the process *Process-With-Main-Event-Loop* is in the
;; debugger, otherwise NIL
(defun Listener-Process-Broken-P ()
  (when *Process-With-Main-Event-Loop*
    #+allegro (not (zerop (mp:symeval-in-stack-group 'tpl::*break-level*
		     (mp:process-stack-group *Process-With-Main-Event-Loop*))))
    #+lucid (lcl:process-in-the-debugger-p *Process-With-Main-Event-Loop*)
    #+lispworks (let ((value (system::read-special-in-sg
			      (mp::process-stack-group
			       *Process-With-Main-Event-Loop*)
			      'system::*debug-level*)))
		  (and (numberp value) (not (zerop value))))
    #-(or allegro lucid lispworks) NIL
    ))


(defun send-timer-event (inter)
  (let* ((wins (Get-Interactor-Windows inter))
	 (win (if (listp wins) (car wins) wins)))
    (if-debug inter (Format T "Posting Timer event for ~s~%" inter))
    (when win
      (let ((indx (g-value inter :timer-array-index)))
	(unless indx
	  (vector-push-extend inter *Inters-With-Timers* 10)
	  ;; vector-push-extend is supposed to return the old value of
	  ;; the fill-pointer but under HP Lucid it doesn't, so just
	  ;; explicitly use the new value of the fill pointer to be safe.
	  (setq indx (1- (fill-pointer *Inters-With-Timers*)))
	  (s-value inter :timer-array-index indx))
	#+garnet-debug;; only test when debugging
	(unless (eq (g-value inter :timer-array-index)
		    (position inter *inters-with-timers*))
	  (error "Interactor timer index not eq to position for ~s" inter))
	(gem:inject-event win indx)
	(gem:flush-output win)))))


;;;Sleep for appropriate time (in seconds), and then wake up and send event
(defun Timer-Process-Main-Loop (inter time once)
  (loop
   #-(and cmu mp) (sleep time)
   #+(and cmu mp) (mp:process-wait-with-timeout
		   "Timer Sleep" time (constantly nil))
   (unless (schema-p inter)  ;; if inter destroyed somehow
     (return))
   (when (Listener-Process-Broken-P) ;; if main-event-loop crashed
     (if-debug inter
      (Format T "Main event loop in debugger so aborting anim for ~s~%" inter))
     (return))
   (send-timer-event inter)
   (when once (return))
   ;; now, make sure other processes run
   #+allegro   (mp:process-allow-schedule)  
   #+lucid     (lcl:process-allow-schedule)
   #+lispworks (mp:process-allow-scheduling)
   #+(and cmu mp) (mp:process-yield)
   ))

;;; Kills the timer process for the interactor, if any
(defun kill-timer-process (inter)
  (let ((timer-process (g-value inter :timer-event-process)))
    (when timer-process (internal-kill-timer-process timer-process))
    (s-value inter :timer-event-process NIL)))

#-garnet-processes ;; not possible in other lisps
(defun launch-timer-process (inter time once)
  "This only works in Lucid, Allegro and LispWorks CL"
  (declare (ignore inter time once))
  )
  
#+garnet-processes
(defun launch-timer-process (inter time once)
  "Spawn a process which is waiting for timer events"

  (let ((timer-process (g-value inter :timer-event-process)))
    (when timer-process (internal-kill-timer-process timer-process))
    ;;; DZG (xlib:intern-atom opal::*default-x-display* ':TIMER_EVENT)
    (setf timer-process
	  #+allegro
	  (mp:process-run-function
	   "Garnet Timer"
	   ;; Use a lambda to pass the parameters.
	   ;; This runs at priority 0, so it will be
	   ;; lower than main-event-loop-process.
	   #'(lambda ()
	       (Timer-Process-Main-Loop inter time once)))
	  #+lucid 
	  (lcl:make-process :name "Garnet Timer"
			    :priority 200
			    :function
			    ;; Use a lambda to pass the parameters.
			    ;; This runs at priority 200, so it will be
			    ;; lower than main-event-loop-process.
			    #'(lambda ()
				(Timer-Process-Main-Loop inter time once)))
	  #+lispworks
	  (mp:process-run-function
	   "Garnet Timer"
	   `(:name "Garnet Timer")
	   ;; Use a lambda to pass the parameters.
	   ;; This runs at priority 0, so it will be
	   ;; lower than main-event-loop-process.
	   #'(lambda ()
	       (Timer-Process-Main-Loop inter time once)))
	  #+(and cmu mp)
	  (mp:make-process
	   ;; Use a lambda to pass the parameters.
	   ;; This runs at priority 200, so it will be
	   ;; lower than main-event-loop-process.
	   #'(lambda ()
	       (Timer-Process-Main-Loop inter time once))
	   :name "Garnet Timer"))
    (if-debug inter (format T "Launching process ~s for ~s~%" timer-process
			    inter))
    (s-value inter :timer-event-process timer-process)
    (push timer-process *All-Timer-Processes*)
    timer-process))


;; This is called when a timer event occurs for the interactor
(defun Handle-Timer-Event (inter-index)
  (let ((inter (aref *Inters-With-Timers* inter-index)))
    (when (and inter (schema-p inter))
      (if-debug inter (Format T "Timer event for ~s~%" inter))
      (if (eq (g-value inter :current-state) :start)
	  (kill-timer-process inter) ; whoops, process shouldn't be running
	  (progn
	    (kr-send inter :timer-handler inter)
	    (opal:update-all))))))


;;;-------------------------------------------------------------------------

#+(or allegro lispworks)
(defun internal-kill-timer-process (timer-process)
  (when (eq (type-of timer-process) 'mp:process)
    (mp:process-kill timer-process)
    (deleteplace timer-process *All-Timer-Processes*)
    ))

#+lucid
(defun internal-kill-timer-process (timer-process)
  (when (and timer-process (lcl:processp timer-process))
    (lcl:kill-process timer-process)
    (deleteplace timer-process *All-Timer-Processes*)
    ))

#+(and cmu mp)
(defun internal-kill-timer-process (timer-process)
  (when (mp:processp timer-process)
    (mp:destroy-process timer-process)
    (deleteplace timer-process *All-Timer-Processes*)
    (mp:process-yield)))

#-(or allegro lucid lispworks (and cmu mp))
(defun internal-kill-timer-process (timer-process)
  (declare (ignore timer-process))
  )
