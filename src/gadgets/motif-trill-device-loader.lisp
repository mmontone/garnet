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
;;;  MOTIF-TRILL-DEVICE-LOADER:  Loads the modules "motif-h-scroll-bar"
;;;                              and "motif-trill-device"
;;;
;;; Change log:
;;; Rajan Parthasarathy - Created

(in-package "USER")

(unless (boundp 'Garnet-Gadgets-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Gadgets-PathName before
  loading Gadgets."))

(unless (get :garnet-modules :motif-trill-device)
  (format t "Loading Motif-Trill-Device...~%")
  (dolist (pair '((:motif-h-scroll-bar "motif-h-scroll-loader")
		  (:motif-trill-device "motif-trill-device")
		  ))
    (unless (get :garnet-modules (car pair))
      (load (user::garnet-pathnames (cadr pair)
			     #+cmu "gadgets:"
			     #+(not cmu) Garnet-Gadgets-PathName)
	    :verbose T)))
  (format t "...Done Motif-Trill-Device.~%"))

(setf (get :garnet-modules :motif-trill-device) T)