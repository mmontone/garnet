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
;;;
;;; Changes:
;;; 12-Sep-92 Mickish    Added "kr-macros"
;;; 10-Apr-92 Pervin	 Added in-package.
;;; 20-Jan-92 Mickish    Moved make-package calls into garnet-loader
;;; 17-Jan-92 Pervin	 Load compiled files, but don't call provide, in CMUCL,
;;; 26-Mar-91 Pervin     Load compiled files in Lucid.
;;; 22-Mar-91 Pervin     Added provide and setf
;;; 05-Jun-90 Richardson  Added lispworks
;;; 12-Apr-90 Mitchell   Added #+allegro (gc t)
;;; 22-Mar-90 Robert Cook Define the "KR" and "KR-DEBUG" packages
;;;			   for the TI Explorer

(in-package "USER")

(Defparameter Garnet-KR-Files
  '(
        "kr-macros" "kr" "constraints"
	))

(dolist (file Garnet-KR-Files)
  (compile-file (user::garnet-pathnames file Garnet-KR-Src))
  (load (user::garnet-pathnames file Garnet-KR-Src)))


(setf (get :garnet-modules :kr) T)

#+allegro-V3.1 (gc t)

