;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;;; 
;;;___________________________________________________________________
;;; The Garnet User Interface Development Environment
;;; Copyright (c) 1991 Carnegie Mellon University
;;; All rights reserved.  The CMU software License Agreement specifies
;;; the terms and conditions for use and redistribution.
;;;
;;; If you want to use this code or anything developed as part of the
;;; Garnet project, please contact Brad Myers (Brad.Myers@CS.CMU.EDU).
;;;___________________________________________________________________
;;;

#|
==================================================================
Change log:
    5/25/93 Dave Kosbie - Removed references to "kr-extra" (placed
                          #'kr::i-depend-on inside actual KR code)
    2/24/93 Andrew Mickish - Removed references to compile-opal/inter-p
    4/14/91 Brad Myers - created
==================================================================
|#

(in-package "USER")

(format t "Compiling C32...~%")

;; check first to see if place is set
(unless (boundp 'Garnet-C32-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-C32-PathName before loading this file."))

;; Load files that C32 depends on, only if not compiling the rest of Garnet
;; already

(unless (get :garnet-modules :multifont)
  (garnet-load "opal:multifont-loader"))

(unless (get :garnet-modules :gilt-functions)
  (garnet-load "gilt:gilt-functions-loader"))

(dolist (gadget '(
		  "labeled-box-loader"
		  "x-buttons-loader"
		  "arrow-line-loader"
		  "text-buttons-loader"
		  "scrolling-window-loader"
		  "scrolling-input-string-loader"
		  "scrolling-menu-loader"  ; for pop-up-functions
		  "error-gadget-loader"	  		; for C32error
		  "scrolling-labeled-box-loader"	; for package name
		  "motif-scrolling-window-loader"
		  ))
  (garnet-load (concatenate 'string "gadgets:" gadget)))

(defvar C32-files '(
		    "c32"
		    "c32formula"
		    "c32ref"
		    "pop-up-generalize"
		    "pop-up-copy-formula"
		    "pop-up-ask-object"
		    "pop-up-functions"
		    "c32dialog"
		    "c32-lapidary"
		    ))

(dolist (file c32-files)
  (garnet-compile (concatenate 'string "c32:" file))
  (garnet-load (concatenate 'string "c32:" file))
  )


#+(or allegro explorer lispworks lucid)
(setf (get :garnet-modules :C32) t)

(format t "... Done Compiling C32~%")
