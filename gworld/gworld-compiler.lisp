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
;;; Changes:
;;; 03/16/94 Mickish - Created

(in-package "USER")

(defvar Gworld-Version-Number "1.0")

(format t "Compiling Gworld...~%")

;;; check to see if pathname variable is set
(unless (boundp 'Garnet-Gworld-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Gworld-PathName before loading gem."))

;;;  Compile Gworld  ...
(Defparameter Garnet-Gworld-Files
  '(
    "Utility:code:gcable-macptrs"
    "Utility:code:utility"
    "Sheet:substrate:pixmaps"
    "Sheet:substrate:gworld"
    ))

(dolist (file Garnet-Gworld-Files)
  (compile-file (user::garnet-pathnames file Garnet-Gworld-Pathname))
  (load (user::garnet-pathnames file Garnet-Gworld-Pathname))
  #+comment
  (garnet-compile (concatenate 'string "gworld:" file)))

(setf (get :garnet-modules :gworld) t)
(format t "...Done Gworld.~%")
