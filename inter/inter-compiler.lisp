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
;;; 26-May-93 Mickish/Goldberg  Added lispkeyhandling
;;; 24-Feb-93 Mickish   Make sure multifont is loaded before compiling;
;;;                     note that multifont has been loaded at end.
;;;  5-Feb-93 Mickish   Added load of multifont
;;;  4-Jun-92 Myers     New animation interactor
;;; 29-May-92 Pervin    New animation-process
;;; 19-May-92 Pervin    Removed call to install-new-char-reader.
;;;  2-Apr-92 McDaniel  New multifont.
;;; 20-Jan-92 Mickish   Moved make-package call into Garnet-Loader
;;; 18-Jun-91 Pervin	Added multifont-textinter.
;;; 26-Mar-91 Pervin    Changes for kcl.
;;; 26-Mar-91 Pervin    Load compiled files in Lucid.
;;; 22-Mar-91 Pervin    Added #-cmu setf and provide at end.
;;; 12-Apr-90 Mitchell  Added #+allegro (gc t)
;;; 3/22/90 Robert Cook Define the "INTERACTORS" package for the
;;;			TI Explorer

(in-package "USER")

;; Only loads this file when not compiling all of Garnet.
(unless (or (and (boundp 'user::compile-opal-p) user::compile-opal-p)
            (boundp 'opal::multifont-text))
  (load (user::garnet-pathnames "multifont" user::Garnet-Opal-PathName)))


(Defvar Garnet-Inter-Files   ;; defvar rather than defparameter so can setq
			     ;; this variable before loading if only want
			     ;; to compile some of these files
  '(
	    ;; key translation files
    "garnet-keytrans"
    "define-mouse-keys"

    #-apple "x-define-keys"
    #-apple "x-inter"

    #+apple "mac-define-keys"
    #+apple "mac-inter"

	    ;; interactor files
    "interactors"
    "accelerators"
    "animation-process"
    "i-windows"
    "menuinter"
    "movegrowinter"
    "buttoninter"
    "twopointinter"
    "textkeyhandling"
    "lispkeyhandling"
    "textinter"
    "multifont-textinter"
    "focus-multifont-textinter"
    "selection-interactor"
    "angleinter"
    "animatorinter"))

(dolist (file Garnet-Inter-Files)
  (compile-file (user::garnet-pathnames file Garnet-Inter-Src))
  (load (user::garnet-pathnames file Garnet-Inter-Src)))

#+allegro-V3.1 (gc t)

(setf (get :garnet-modules :inter) t)
(setf (get :garnet-modules :multifont) t)

