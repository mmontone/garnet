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

;;;
;;; Changes:
;;;
;;; 09/21/92 Andrew Mickish - Added load of dependent gadgets
;;; 04/01/92 Andrew Mickish - Renamed ggt to agate
;;; 03/26/92 Ed Pervin - Don't load ggt, because that loads gesture-loader.
;;; 03/24/92 James Landay   - added ggt (note: it doesn't auto load)
;;; 02/21/92 James Landay   - added train (note: it doesn't auto load)
;;; 02/20/92 Andrew Mickish - Created
;;;

(in-package :USER)

(dolist (pair '((:motif-text-buttons "motif-text-buttons-loader")
		(:motif-scrolling-labeled-box "motif-scrolling-labeled-box-loader")
		(:motif-radio-buttons "motif-radio-buttons-loader")
		(:motif-error-gadget "motif-error-gadget-loader")
                (:motif-save-gadget "motif-save-gadget-loader")
                (:motif-scrolling-window "motif-scrolling-window-loader")))
  (unless (get :garnet-modules (car pair))
    (garnet-load (concatenate 'string "gadgets:" (cadr pair)))))

(defpackage :AGATE (:use :KR :INTER :COMMON-LISP)
  (:export DO-GO DO-STOP))

(Defvar Garnet-Gesture-Files
  '(
    "features" 
    "matrix"
    "classify" 
    "gestureinter" 
    "fileio" 
    "train"
    "agate"
    ))

(dolist (file Garnet-Gesture-Files)
  (compile-file (user::garnet-pathnames file Garnet-Gesture-Src))
  (unless (string= file "agate")
    (load (user::garnet-pathnames file Garnet-Gesture-Src))))

#+allegro-V3.1 (gc t)

(setf (get :garnet-modules :gesture) t)

