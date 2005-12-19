;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-

;;; Created 6/25/92 RGA

;;; Loader for protected eval stuff.

(in-package "COMMON-LISP-USER")

(unless (get :garnet-modules :prompter)
  (format t "Loading Prompter~%")
  
  (dolist (pair '((:error-gadget "error-gadget-loader")))
    (unless (get :garnet-modules (car pair))
      (load (merge-pathnames (cadr pair) Garnet-Gadgets-PathName)
	    :verbose T)))
  
  (dolist (pair '((:protected-eval "protected-eval:protected-eval-loader")
		  (:scrolling-unlabeled-box "gadgets:scrolling-unlabeled-box-loader")
		  (:prompter "protected-eval:prompter")
		  ))
    (unless (get :garnet-modules (car pair))
      (garnet-load (cadr pair))))
  (format t "...Done Prompter.~%"))

(setf (get :garnet-modules :prompter) t)

