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
;;; 14-May-93 Mickish     Added new files, split off from others
;;; 20-Jan-92 Mickish     Removed make-package call
;;; 26-Mar-91 Pervin      Load compiled files in Lucid.
;;; 22-Mar-91 Pervin      Added setf of :garnet-modules, and provides at end.
;;; 19-Mar-91 Pervin      Added aggregraphs.
;;; 4-Mar-91 D'Souza      Removed nickname "MO" of Opal.
;;; 5-Jun-90 Richardson   Added lispworks
;;; 8-May-90 Dannenberg   Added new files
;;; 16-Apr-90 Pervin      Changed #+explorer to #+(or allegro explorer)
;;; 12-Apr-90 Mitchell    Added #+allegro (gc t)
;;; 3/22/90 Robert Cook - Define the package "OPAL" for the TI Explorer

(in-package "USER")

(Defvar Garnet-Aggregadgets-Files
  '(
    "agg-macros"
    "agg-utils"
    "aggregadgets"
    "aggrelists"
    "add-agg"
    "agg-fix-slots"
    "copy-agg"
    "save-agg"
    "string-edit"
    "agg-labels"
    "rectangle-conflict-object"
    "aggregraphs"
    "scalable-aggregraph"
    "scalable-aggregraph-image"
    ))


(dolist (file Garnet-Aggregadgets-Files)
  (compile-file (user::garnet-pathnames file Garnet-Aggregadgets-Src))
  (load (user::garnet-pathnames file Garnet-Aggregadgets-Src)))

#+allegro-V3.1 (gc t)

(setf (get :garnet-modules :aggregadgets) t)
(setf (get :garnet-modules :aggregraphs) t)
