;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-
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
;;; 10/2/03 RGA --- New compile/load protocol
        7/28/96 RGA --- changed to use garnet-compile/load
        4/ 5/93 Dave Kosbie - created
============================================================
|#

(in-package "COMMON-LISP-USER")

(eval-when (eval load compile)
  (garnet-mkdir-if-needed Garnet-Utils-Pathname))

(Defparameter Garnet-Utils-Files
  '(
        "general"
	))

(dolist (file Garnet-Utils-Files)
  (let ((gfile (concatenate 'string "utils:" file)))
    (garnet-compile gfile)
    (garnet-load gfile)))

(garnet-copy-files Garnet-Utils-Src Garnet-Utils-Pathname
		   '("utils-loader.lisp"))


(setf (get :garnet-modules :utils) T)

#+allegro-V3.1 (gc t)

