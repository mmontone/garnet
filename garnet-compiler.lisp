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
;;; This file  compiles all the garnet modules.
;;; First load the file: 	garnet-prepare-compile
;;; Then load 			garnet-loader
;;; Then load this file:	garnet-compiler
;;;
;;; ** See the comments at the top of garnet-prepare-compile
;;;
;;;

#|
============================================================
Change log:
         03/16/94 Andrew Mickish - Added Gworld module for Mac
         11/01/93 Andrew Mickish - Added GEM
         10/14/93 Andrew Mickish - Removed Lucid compiler proclamation
         05/12/93 Dave Kosbie - Moved gadgets before debug
         04/15/93 Andrew Mickish - Added lucid memory-management instruction
         04/05/93 Dave Kosbie - Added Garnet-Utils
         07/23/92 Dario Giuse - Moved C32 before Lapidary
         06/24/92 Andrew Mickish - Added C32
	 04/02/92 McDaniel - new multifont
         02/20/92 Andrew Mickish - Added gestures
         03/11/91 Andrew Mickish - Added aggregraphs
         12/5/89 Brad Myers - Fixed so works when files are in /src directories
         10/30/89 Brad Myers - Added debug, changed names
         8/18/89 Brad Myers - Added Gadgets
         6/21/89 Brad Myers - Created
============================================================
|#

(in-package "USER")

;;; Get rid of forward reference warnings in Lucid.
#+lucid
(compiler-options :undef-warnings nil)

(unless (and (boundp 'load-utils-p-copy)        (boundp 'Garnet-Utils-Src)
             (boundp 'load-kr-p-copy)           (boundp 'Garnet-KR-Src)
	     (boundp 'load-gworld-p-copy)       (boundp 'Garnet-Gworld-Src)
	     (boundp 'load-gem-p-copy)          (boundp 'Garnet-Gem-Src)
	     (boundp 'load-opal-p-copy)         (boundp 'Garnet-Opal-Src)
	     (boundp 'load-inter-p-copy)        (boundp 'Garnet-Inter-Src)
	     (boundp 'load-multifont-p-copy)
	     (boundp 'load-ps-p-copy)           (boundp 'Garnet-PS-Src)
	     (boundp 'load-aggregadgets-p-copy) (boundp 'Garnet-Aggregadgets-Src)
	     (boundp 'load-aggregraphs-p-copy)  
	     (boundp 'load-debug-p-copy)        (boundp 'Garnet-Debug-Src)
	     (boundp 'load-gadgets-p-copy)      (boundp 'Garnet-Gadgets-Src)
	     #+garnet-protected-eval
	     (boundp 'load-protected-eval-p-copy)
	     #+garnet-protected-eval	     
	     (boundp 'Garnet-Protected-Eval-Src)
	     (boundp 'load-gesture-p-copy)      (boundp 'Garnet-Gesture-Src)
	     (boundp 'load-demos-p-copy)        (boundp 'Garnet-Demos-Src)
	     (boundp 'load-C32-p-copy)          (boundp 'Garnet-C32-Src)
	     (boundp 'load-lapidary-p-copy)     (boundp 'Garnet-Lapidary-Src)
	     (boundp 'load-gilt-p-copy)         (boundp 'Garnet-Gilt-Src)
	     )
  (error "** Must load Garnet-Prepare-Compile and Garnet-Loader before
  loading this file"))

(when compile-utils-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling Utils %%%%%%%%%%%%%%% ~%")
  (load (merge-pathnames "utils-compiler" Garnet-Utils-Src)))
(unless compile-utils-p
  (load Garnet-Utils-Loader))


(when compile-kr-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling KR %%%%%%%%%%%%%%% ~%")
  (load (merge-pathnames "kr-compiler" Garnet-KR-Src)))
(unless compile-kr-p
  (load Garnet-KR-Loader))


#+apple
(when compile-gworld-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling Gworld %%%%%%%%%%%%%%% ~%")
  (load (merge-pathnames "gworld-compiler" Garnet-Gworld-Src)))
#+apple
(unless compile-gworld-p
  (load Garnet-Gworld-Loader))


(when compile-gem-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling Gem %%%%%%%%%%%%%%% ~%")
  (load (merge-pathnames "gem-compiler" Garnet-Gem-Src)))
(unless compile-gem-p
  (load Garnet-Gem-Loader))


(when compile-opal-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling Opal %%%%%%%%%%%%%%% ~%")
  (load (merge-pathnames "opal-compiler" Garnet-Opal-Src)))
(unless compile-opal-p
  (load Garnet-Opal-Loader))


(when compile-inter-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling Inter %%%%%%%%%%%%%%% ~%")
  (load (merge-pathnames "inter-compiler" Garnet-Inter-Src)))
(unless compile-inter-p
  (load Garnet-Inter-Loader))  ; have to load this to go on


(when compile-PS-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling PS %%%%%%%%%%%%%%% ~%")
  (load (merge-pathnames "ps-compiler" Garnet-PS-Src)))
(unless compile-PS-p
  (load Garnet-PS-Loader))  ; have to load this to go on


(when compile-aggregadgets-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling Aggregadgets %%%%%%%%%%%%%%% ~%")
  (load (merge-pathnames "aggregadgets-compiler" Garnet-Aggregadgets-Src)))
(when (or load-aggregadgets-p-copy compile-demos-p
	     compile-lapidary-p compile-gadgets-p)
  (unless compile-aggregadgets-p
    (load Garnet-Aggregadgets-Loader)))  ; need this if compile demos, gadgets,
				         ; or lapidary


(when compile-gadgets-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling Gadgets %%%%%%%%%%%%%%% ~%")
  (load (merge-pathnames "gadgets-compiler" Garnet-Gadgets-Src)))
(when (or load-gadgets-p-copy #+garnet-protected-eval compile-protected-eval-p compile-demos-p compile-lapidary-p)
  (unless compile-gadgets-p
    (load Garnet-Gadgets-Loader)))

#+garnet-protected-eval
(when compile-protected-eval-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling Protected-eval %%%%%%%%%%%%%%% ~%")
  (load (merge-pathnames "protected-eval-compiler" Garnet-Protected-Eval-Src)))
#+garnet-protected-eval
(unless compile-protected-eval-p
  (load Garnet-Protected-Eval-Loader)) ; have to load this to go on


(when compile-debug-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling Debugging Routines %%%%%%%%%%%%% ~%")
  (load (merge-pathnames "debug-compiler" Garnet-Debug-Src)))
(when load-debug-p-copy
  (unless compile-debug-p
    (load Garnet-Debug-Loader)))


(when compile-gesture-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling Gestures %%%%%%%%%%%%%%% ~%")
  (load (merge-pathnames "gesture-compiler" Garnet-Gesture-Src)))
(unless compile-gesture-p
  (load Garnet-Gesture-Loader))  ; have to load this to go on


(when compile-demos-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling Demos %%%%%%%%%%%%%%% ~%")
  (load (merge-pathnames "demos-compiler" Garnet-Demos-Src)))
(when load-demos-p-copy
  (unless compile-demos-p
    (load Garnet-Demos-Loader)))


(when compile-gilt-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling Gilt %%%%%%%%%%%%%%% ~%")
  (load (merge-pathnames "gilt-compiler" Garnet-Gilt-Src)))
(when load-gilt-p-copy
  (unless compile-gilt-p
    (load Garnet-Gilt-Loader)))


(when compile-C32-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling C32 %%%%%%%%%%%%%%% ~%")
  (load (merge-pathnames "c32-compiler" Garnet-C32-Src)))
(when load-C32-p-copy
  (unless compile-C32-p
    (load Garnet-C32-Loader)))


(when compile-lapidary-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling Lapidary %%%%%%%%%%%%%%% ~%")
  (load (merge-pathnames "lapidary-compiler" Garnet-Lapidary-Src)))
(when load-lapidary-p-copy
  (unless compile-lapidary-p
    (load Garnet-Lapidary-Loader)))


(setf *Garnet-Going-To-Compile* NIL)  ; no longer in compile mode
