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
;;; 10/2/03 RGA --- New compile/load protocol
;;;    7/28/96 RGA --- changed to use garnet-compile/load
;;; 02/24/93 Andrew Mickish - Removed references to compile-opal/inter-p
;;; 04/15/92 Mickish - Added ps-multifont and load of multifont-loader
;;;

(in-package "USER")

;; Only loads this file when not compiling all of Garnet.
(unless (get :garnet-modules :multifont)
  (load (garnet-pathnames "multifont-loader"
			 #+cmu "opal:"
			 #+(not cmu) Garnet-Opal-PathName)))

(garnet-mkdir-if-needed Garnet-ps-Pathname)

(Defparameter Garnet-PS-Files
  '(
    "ps"
    "ps-multifont"
    ))

(dolist (file Garnet-PS-Files)
  (let ((gfile (concatenate 'string "ps:" file)))
    (garnet-compile gfile)
    (garnet-load gfile)))

(garnet-copy-files Garnet-Ps-Src Garnet-Ps-Pathname
		   '("ps-loader.lisp"))

(setf (get :garnet-modules :ps) T)
