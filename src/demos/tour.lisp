;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is the loader file for the special Garnet Tour


;;; CHANGE LOG
;;; 05/30/94  Marty Geier - Changed loads to garnet-load
;;;                         commented out defpackage :user, causes error



(in-package :USER)
(use-package '(:COMMON-LISP :KR :KR-DEBUG :GARNET-DEBUG))

(defvar MYWINDOW NIL)

(dolist (file '("radio-buttons-loader" 
                "v-slider-loader"
                "menu-loader"
                "v-scroll-loader"))
   (user::garnet-load (concatenate 'string "gadgets:" file)))

(dolist (file '("mge" "demo-othello"))
   (user::garnet-load (concatenate 'string "demos:" file)))


; Old loads kept below just in case                
#|
(user::garnet-load (user::garnet-pathnames "radio-buttons-loader" user::Garnet-Gadgets-PathName)
      :verbose T)
(load (user::garnet-pathnames "v-slider-loader" user::Garnet-Gadgets-PathName) :verbose T)
(load (user::garnet-pathnames "menu-loader" user::Garnet-Gadgets-PathName) :verbose T)
(load (user::garnet-pathnames "v-scroll-loader" user::Garnet-Gadgets-PathName) :verbose T)

(load (user::garnet-pathnames "mge" Garnet-Demos-PathName) :verbose T)
(load (user::garnet-pathnames "demo-othello" Garnet-Demos-PathName) :verbose T)
|#


#+allegro-v4.0 (unintern 'path (find-package "COMMON-LISP-USER"))
(create-instance 'moving-rectangle opal:rectangle
		 (:box '(80 20 100 150))
		 (:left (o-formula (first (gvl :box))))
		 (:top (o-formula (second (gvl :box))))
		 (:width (o-formula (third (gvl :box))))
		 (:height (o-formula (fourth (gvl :box)))))

(defun start-othello ()
  (demo-othello:do-go)
  T)

(defun start-editing ()
  (mge:editor-show-window)
  T)

(defun stop-othello ()
  (demo-othello:do-stop)
  "Bye-bye from Othello")

(defun stop-tour ()
  (when (schema-p mywindow)
    (opal:destroy mywindow))
  (demo-othello:do-stop)  ; this stops mge also
  "Thank you for your interest in the Garnet Project")

(Format T "~%~%Garnet-Tour Load Complete.~%
Welcome to the Garnet Tour.  You can now start typing.~%")
