;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  H-SCROLL-LOADER:  Loads the module "h-scroll-bar" and "parts"
;;;                    modules if necessary

#|
==================================================================
Change log:
    03/22/90 Robert Cook - Define the package "GARNET-GADGETS"
                           for the TI Explorer
    01/30/89 Andrew Mickish - Added check before loading h-scroll-bar
    10/19/89 Andrew Mickish - Created
==================================================================
|#

(in-package "USER")

;; check first to see if place is set
(unless (boundp 'Garnet-Gadgets-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Gadgets-PathName before
  loading Gadgets."))

(unless (get :garnet-modules :h-scroll-bar)
  (format t "Loading H-Scroll-Bar...~%")
  (dolist (pair '((:GAD-scroll-parts "GAD-scroll-parts")
		  (:GAD-h-arrows "GAD-h-arrows")
		  (:GAD-h-boxes "GAD-h-boxes")
		  (:h-scroll-bar "h-scroll-bar")))
    (unless (get :garnet-modules (car pair))
      (load (user::garnet-pathnames (cadr pair)
			     #+cmu "gadgets:"
			     #+(not cmu) Garnet-Gadgets-PathName)
	    :verbose T)))
  (format t "...Done H-Scroll-Bar.~%"))


(setf (get :garnet-modules :h-scroll-bar) t)



