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
;;;  1-Nov-93 Mickish - Created

(in-package "USER")

(Defvar Garnet-Gem-Files
  '(
    "gem"
    "define-methods"
    ))

(dolist (file Garnet-Gem-Files)
  (compile-file (user::garnet-pathnames file Garnet-Gem-Src))
  (load (user::garnet-pathnames file Garnet-Gem-Src)))

(setf (get :garnet-modules :gem) T)

#+allegro-V3.1 (gc t)

