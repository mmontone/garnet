;;;
;;; Compile protected eval stuff as Garnet module. 
;;; Derived from opal version of this file.

(defparameter Garnet-Protected-Eval-Files
  '(
    "protected-eval:scrolling-unlabeled-box"
    "protected-eval:prompter"
    "protected-eval:protected-eval"
    "protected-eval:protected-process"))

(dolist (file Garnet-Protected-Eval-Files)
  (compile-file (user::garnet-pathnames file Garnet-Protected-Eval-Src))
  (load (user::garnet-pathnames file Garnet-Protected-Eval-Src)))

(setf (get :garnet-modules :protected-eval) T)
