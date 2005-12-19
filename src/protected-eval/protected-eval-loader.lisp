;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-

;;; 10/2/03 RGA --- Moved to new home.
;;; Created 6/25/92 RGA

;;; Loader for protected eval stuff.

(in-package "COMMON-LISP-USER")

;; check first to see if place is set
(unless (boundp 'Garnet-Contrib-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Contrib-PathName
before loading Contributed Gadgets."))


(unless (get :garnet-modules :new-protected-eval)
  (format t "Loading New Protected-eval~%")
  
  (dolist (pair '((:error-gadget "error-gadget-loader")))
    (unless (get :garnet-modules (car pair))
      (load (merge-pathnames (cadr pair) Garnet-Gadgets-PathName)
	    :verbose T)))
  
  (dolist (pair '((:protected-eval "protected-eval:base-protected-eval-loader")
		  (:scrolling-unlabeled-box "gadgets:scrolling-unlabeled-box-loader")
		  (:prompter "protected-eval:prompter-loader")
		  (:new-protected-eval "protected-eval:new-protected-eval")
		  ))
    (unless (get :garnet-modules (car pair))
      (garnet-load (cadr pair))))
  
  (format t "...Done New Protected Eval.~%"))

(setf (get :garnet-modules :new-protected-eval) t)




