;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

#|
==================================================================
Change log:
   11/05/90 Andrew Mickish - created
==================================================================
|#

(in-package "USER")

;; check first to see if pathname variable is set
(unless (boundp 'Garnet-Gilt-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Gilt-PathName before loading this file."))

;;; Now load the Filter-Functions module
;;;
(unless (get :garnet-modules :Filter-Functions)
  (format t "Loading Filter functions...~%")
  (load (user::garnet-pathnames "filter-functions"
			 #+cmu "gilt:"
			 #+(not cmu) Garnet-Gilt-PathName)
	:verbose T)
  (format t "...Done Filter Functions.~%"))

(setf (get :garnet-modules :Filter-Functions) t)


