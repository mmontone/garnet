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
        9/12/92 Dario Giuse     Added "kr-macros"
	3/22/90 Robert Cook     Define the packages "KR" and
				"KR-DEBUG" for the TI Explorer
        1/4/90  Ed Pervin       added version number
	6/14/89 Dario Giuse	created
============================================================
|#

(in-package "USER")

(defparameter KR-Version-Number "2.3.4")

(format t "Loading KR...~%")

;; check first to see if pathname variable is set
(unless (boundp 'Garnet-Kr-PathName)
  (error
   "Load 'Garnet-Loader' first to set Garnet-Kr-PathName before loading KR."))

(Defparameter Garnet-Kr-Files
  '("kr-macros" "kr" "constraints"))

(dolist (file Garnet-Kr-Files)
  (load (user::garnet-pathnames file Garnet-Kr-PathName)
	:verbose T))

(setf (get :garnet-modules :kr)  t)
(format t "...Done Kr.~%")
