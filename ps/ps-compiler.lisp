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
;;; CHANGE LOG:
;;; 02/24/93 Andrew Mickish - Removed references to compile-opal/inter-p
;;; 04/15/92 Mickish - Added ps-multifont and load of multifont-loader
;;;

(in-package "USER")

;; Only loads this file when not compiling all of Garnet.
(unless (get :garnet-modules :multifont)
  (load (user::garnet-pathnames "multifont-loader"
			 #+cmu "opal:"
			 #+(not cmu) user::Garnet-Opal-PathName)))

(Defparameter Garnet-PS-Files
  '(
    "ps"
    "ps-multifont"
    ))

(dolist (file Garnet-PS-Files)
  (compile-file (user::garnet-pathnames file Garnet-PS-Src))
  (load (user::garnet-pathnames file Garnet-PS-Src)))


(setf (get :garnet-modules :ps) T)
