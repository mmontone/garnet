;;; RGA  Compile script for prompter/protected/eval/etc.
;;; Assumes that my hacks to the contrib directory stuff are in place.
;;; 

;;; To compile, it may be necessary to copy the entire contib/prompter
;;; subdirectory from src/ into bin/.  Then this compile script will be able
;;; to load dependent files as necessary.  Then simply load this file.

;;; 8/31/93 RGA Added search path stuff for CMU lisp.

(defvar PROMPTER-SRC
  (merge-pathnames "prompter/" Garnet-Contrib-Src))
(defvar PROMPTER-PATHNAME
  (merge-pathnames "prompter/" Garnet-Contrib-PathName))
(user::Add-Garnet-Load-Prefix "prompter-src" PROMPTER-SRC)
(user::Add-Garnet-Load-Prefix "prompter" PROMPTER-PATHNAME)

(let ((Garnet-Contrib-Pathname Garnet-Contrib-Src))
  #+cmu (setf (ext:search-list "prompter-src:") (list (namestring PROMPTER-SRC)))
  #+cmu (setf (ext:search-list "prompter:") (ext:search-list "prompter-src:"))
  (user::garnet-load "prompter:new-protected-eval-loader"))
#+cmu
(setf (ext:search-list "prompter:") (list (namestring PROMPTER-PATHNAME)))

;(user::garnet-compile "prompter:new-types")
(user::garnet-compile "prompter:protected-eval")
(user::garnet-compile "prompter:scrolling-unlabeled-box")
(user::garnet-compile "prompter:prompter")
(user::garnet-compile "prompter:new-protected-eval")
(user::garnet-compile "prompter:protected-process")


(opal:shell-exec (format nil "cp ~A*-loader.lisp ~A~%"
			 PROMPTER-SRC PROMPTER-PATHNAME))


