;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;;; 
;;; ______________________________________________________________________
;;;
;;; The Garnet User Interface Development Environment
;;; Copyright (c) 1989, 1990 Carnegie Mellon University
;;; All rights reserved.  The CMU software License Agreement specifies
;;; the terms and conditions for use and redistribution.
;;;
;;; If you want to use this code or anything developed as part of the Garnet
;;; Project, please contact Brad Myers (Brad.Myers@CS.CMU.EDU).
;;; ______________________________________________________________________
;;;

#|
============================================================
Change log:
	4/ 5/93 Dave Kosbie - created
============================================================
|#

(in-package "USER")

(format t "Loading Utils...~%")

;; check first to see if pathname variable is set
(unless (boundp 'Garnet-Utils-PathName)
  (error
   "Load 'Garnet-Loader' first to set Garnet-Utils-PathName before loading Utils."))

(Defparameter Garnet-Utils-Files '(
	"general"
	))

(dolist (file Garnet-Utils-Files)
  (load (user::garnet-pathnames file Garnet-Utils-PathName)
	:verbose T))

(setf (get :garnet-modules :utils)  t)
(format t "...Done Utils.~%")
