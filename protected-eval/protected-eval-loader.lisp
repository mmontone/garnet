;;;
;;; Loader for protected eval stuff.  FMG 15-Nov-2002
;;; Derived from opal version of this file.

(in-package "USER")

(format t "Loading Protected-Eval...~%")

(unless (boundp 'Garnet-Protected-Eval-Pathname)
  (error "Load 'Garnet-Loader' first to set Garnet-Protected-Eval-Pathname
before loading Protected-Eval."))

(dolist (pair '((:motif-error-gadget "motif-error-gadget-loader")
		(:motif-text-buttons "motif-text-buttons-loader")))
  (unless (get :garnet-modules (car pair))
    (load (merge-pathnames (cadr pair) Garnet-Gadgets-PathName)
	  :verbose T)))

(defparameter Garnet-Protected-Eval-Files
  '(
    "protected-eval:scrolling-unlabeled-box"
    "protected-eval:prompter"
    "protected-eval:protected-eval"
    "protected-eval:protected-process"))

  
(dolist (file Garnet-Protected-Eval-Files)
  (load (user::garnet-pathnames file Garnet-Protected-Eval-Pathname)
	:verbose T))


(setf (get :garnet-modules :protected-eval) t)
(format t "...Done Protected Eval.~%"))
