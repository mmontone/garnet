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
;;;  8/27/93 Mickish     Added suggest-constants
;;;  1/10/93 Brad Myers  Added inspector
;;;  2/20/92 Mickish     Moved make-package call into Garnet-Loader
;;;  4/12/90 Mitchell    Added #+allegro (gc t)
;;;  3/22/90 Robert Cook Define the "GARNET-DEBUG" package for the TI Explorer

(in-package "USER")

;; load utilities needed by inspector, unless already loaded

(unless (get :garnet-modules :multifont)
  (load (user::garnet-pathnames "multifont-loader" user::Garnet-Opal-PathName)))
(unless (get :garnet-modules :text-buttons)
  (load (user::garnet-pathnames "text-buttons-loader" user::Garnet-Gadgets-PathName)))
(unless (get :garnet-modules :error-gadget-utils)
  (user::garnet-load "gg:error-gadget-utils"))


(Defvar Garnet-Debug-Files   ;; defvar rather than defparameter so can setq
			     ;; this variable before loading if only want
			     ;; to compile some of these files
  '(
    "debug-fns"
    "objsize"
    "inspector"
    "suggest-constants"))

(dolist (file Garnet-Debug-Files)
  (compile-file (user::garnet-pathnames file Garnet-Debug-Src)))

#+allegroV3.1 (gc t)

(setf (get :garnet-modules :debug) T)
