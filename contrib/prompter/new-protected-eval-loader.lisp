;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-

;;; Created 6/25/92 RGA

;;; Loader for protected eval stuff.

(in-package "USER")

;; check first to see if place is set
(unless (boundp 'Garnet-Contrib-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Contrib-PathName
before loading Contributed Gadgets."))

(defvar PROMPTER-PATHNAME
  (merge-pathnames "prompter/" Garnet-Contrib-PathName))

(unless (get :garnet-modules :new-protected-eval)
  (format t "Loading New Protected-eval~%")
  
  (dolist (pair '((:error-gadget "error-gadget-loader")))
    (unless (get :garnet-modules (car pair))
      (load (merge-pathnames (cadr pair) Garnet-Gadgets-PathName)
	    :verbose T)))
  
  (dolist (pair '((:protected-eval "protected-eval-loader")
		  (:scrolling-unlabeled-box "scrolling-unlabeled-box-loader")
		  (:prompter "prompter-loader")
		  (:new-protected-eval "new-protected-eval")
		  ))
    (unless (get :garnet-modules (car pair))
      (load (merge-pathnames (cadr pair) PROMPTER-PATHNAME)
	    :verbose T)))
  
  (format t "...Done New Protected Eval.~%"))

(setf (get :garnet-modules :new-protected-eval) t)




