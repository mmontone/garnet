;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Changes:
;;;  8-Nov-93 Mickish     Added x.lisp and called gem:init-device
;;;  5-Mar-93 Mickish     Added utils.lisp
;;; 10-Feb-93 Mickish     Added types.lisp
;;; 11-Jun-92 Pervin	  Added pixmaps.lisp.
;;;  7-Apr-92 Pervin      Moved clean-up before windows to eliminate warning.
;;; 20-Mar-92 Pervin	  Added process.lisp.
;;; 20-Jan-92 Mickish     Moved make-package call into Garnet-Loader
;;; 10-Dec-91 Pervin	  Added virtual-aggregates.
;;; 18-Jun-91 Pervin      Added multifont.
;;; 26-Mar-91 Pervin      Load compiled files in Lucid
;;; 22-Mar-91 Pervin      Added #-cmu before setf and provide.
;;;  4-Mar-91 D'Souza     Removed nickname "MO" of Opal
;;; 15-Aug-90 Pervin      Moved clean-up to after open-and-close.
;;;  6-Jun-90 Pervin      Removed *twm-bug*
;;;  5-Jun-90 Richardson  Added lispworks
;;; 12-Apr-90 Mitchell    Added #+allegro (gc t)
;;;  2-Apr-90 Cook/Pervin Added #+explorer part.

(in-package "USER")

(Defparameter Garnet-Opal-Files
  '(
        "types"
        "update-constants"
	"defs"
	"macros"
	"new-defs"
        "utils"
	"text-fonts"
	"create-instances"
	"create-instances2"
        "text-functions"
        "text"

	"update-basics"
	"halftones"
	"objects"
        "roundtangles"
	"basics"
	"aggregates"
        "process"
        "clean-up"
	"windows"
	"update"
        "fast-redraw"
        "update-window"
	"multifont"
        "virtual-aggregates"
	"pixmaps"
        "open-and-close"
        #-apple "x"
        #+apple "mac"))

#+(or ALLEGRO APPLE)
(proclaim '(optimize (debug 0)))

(dolist (file Garnet-Opal-Files)
  (compile-file (user::garnet-pathnames file Garnet-Opal-Src))
  (load (user::garnet-pathnames file Garnet-Opal-Src)))

(when *default-garnet-proclaim*
  (proclaim *default-garnet-proclaim*))

(setf (get :garnet-modules :opal) T)

#+allegro-V3.1 (gc t)

#-apple (gem:init-device :X NIL)
#+apple (gem:init-device :MAC NIL)
