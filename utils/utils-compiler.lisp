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

#|
============================================================
Change log:
        4/ 5/93 Dave Kosbie - created
============================================================
|#

(in-package "USER")

(Defparameter Garnet-Utils-Files
  '(
        "general"
	))

(dolist (file Garnet-Utils-Files)
  (compile-file (user::garnet-pathnames file Garnet-Utils-Src))
  (load (user::garnet-pathnames file Garnet-Utils-Src)))


(setf (get :garnet-modules :utils) T)

#+allegro-V3.1 (gc t)

