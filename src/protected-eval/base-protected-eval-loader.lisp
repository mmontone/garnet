;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-

;;; Created 6/25/92 RGA

;;; Loader for protected eval stuff.

(in-package "USER")

(unless (get :garnet-modules :protected-eval)
  (format t "Loading Protected-Eval~%")
  
  (dolist (pair '((:error-gadget "error-gadget-loader")))
    (unless (get :garnet-modules (car pair))
      (load (merge-pathnames (cadr pair) Garnet-Gadgets-PathName)
	    :verbose T)))
  
  (dolist (pair '(;(:new-types "prompter:new-types")
		  (:protected-eval "protected-eval:protected-eval")))
    (unless (get :garnet-modules (car pair))
      (garnet-load (cadr pair))))
  
  (format t "...Done Protected-Eval Base.~%"))

(setf (get :garnet-modules :new-types) t)
(setf (get :garnet-modules :Protected-Eval) t)

