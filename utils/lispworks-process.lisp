;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;;;
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
        8/19/93 Andrew Mickish - Created
============================================================
|#

(in-package :USER)


;; lispworks-process.lisp
;;
;; This file contains functions copied from the LispWorks Users Guide,
;; version 3.0, Chapter 12.  After executing (guarantee-processes), you
;; will be able to run the main-event-loop background process in LispWorks.
;; Other lisps (e.g., CMUCL, Lucid, and Allegro do not have an analogous
;; requirement).
;;
;; Guarantee-Processes is an alternative to running the big window-oriented
;; LispWorks interface, started with (tools:start-lispworks).  If you do not
;; need all the functionality supplied by the LispWorks environment, and you
;; just want to run the Garnet main-event-loop with the least possible
;; overhead, you should load this file and execute (guarantee-processes)
;; before loading Garnet.  So, for LispWorks, the standard procedure for
;; loading Garnet is:
;;    (load "garnet/src/utils/lispworks-process.lisp")
;;    (guarantee-processes)
;;    (load "garnet/garnet-loader")
;;
;; Guarantee-Processes launches a new listener process that your subsequent
;; commands will be typed into.  The original listener process will stay
;; hung at the (guarantee-processes) call.  Whenever you enter the debugger
;; of this new process (the "base-procees"), you will get restart options
;; that include:
;;
;;       ...
;;       5 (abort) return to level 0.
;;       6 Return to top level
;;       7 Return from multiprocessing
;;
;; When you want to exit the debugger, you should choose either
;; "(abort) return to level 0," or "Return to top level", since both of these
;; options will return you to the top-level LispWorks prompt.  If you ever
;; choose "Return from multiprocessing," then you will kill both the
;; base-process and the main-event-loop-process, and you will have to call
;; (guarantee-processes) and (opal:launch-main-event-loop-process) to start
;; Garnet's main-event-loop-process.
;;
;; You do not need to load this file or execute (guarantee-processes) if you
;; have already done (tools:start-lispworks).

(defun base-process-function ()
  (with-simple-restart
   (abort "Return from multiprocessing")
   (loop
    (with-simple-restart
     (abort "Return to top level")
     (system::%top-level))))		; [undocumented]
  (mp::stop-multiprocessing))

;;; simple startup of multiprocessing with one process (apart from the
;;; idle process) 

(defvar *base-process* '("base-process" nil base-process-function))

;;; (guarantee-processes) will start up multiprocessing with a top
;;; level loop
(defun guarantee-processes ()
  (unless mp::*multiprocessing* 
    (pushnew *base-process*
	     mp:*initial-processes*)
    (mp:initialize-multiprocessing)))
