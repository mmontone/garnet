;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-

;;; RGA  Compile script for prompter/protected-eval/etc.
;;; Assumes that my hacks to the contrib directory stuff are in place.
;;; 

;;; To compile, it may be necessary to copy the entire contib/prompter
;;; subdirectory from src/ into bin/.  Then this compile script will be able
;;; to load dependent files as necessary.  Then simply load this file.

;;; 10/2/03 RGA --- New compile/load protocol
;;; 10/02/03 RGA Moved to protected-eval directory.
;;; 10/02/03 RGA Moved to scrolling-unlabeled-box to gadgets.
;;; 8/31/93 RGA Added search path stuff for CMU lisp.

(garnet-mkdir-if-needed Garnet-Protected-Eval-Pathname)

(garnet-compile "protected-eval:protected-eval")
(garnet-load "protected-eval:protected-eval")
;(garnet-compile "protected-eval:scrolling-unlabeled-box")
(garnet-compile "protected-eval:prompter")
(garnet-load "protected-eval:prompter")
(garnet-compile "protected-eval:new-protected-eval")
(garnet-load "protected-eval:new-protected-eval")
(garnet-compile "protected-eval:protected-process")
(garnet-load "protected-eval:protected-process")

(garnet-copy-files Garnet-Protected-Eval-Src Garnet-Protected-Eval-Pathname
		   '("base-protected-eval-loader.lisp"
		     "prompter-loader.lisp"
		     "protected-eval-loader.lisp"))

