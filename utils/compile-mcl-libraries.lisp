;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
============================================================
Change log:
        5/20/94 Andrew Mickish - Created
============================================================
|#

(in-package :COMMON-LISP-USER)


;; compile-mcl-libraries.lisp
;;
;; This file contains a script for compiling all the MCL library files that
;; are reqired by Garnet.  This script should be executed in MCL before loading
;; Garnet the first time.  You will only need to use this file once, when you
;; first install Garnet.  After that, the compiled library files will always be
;; around and you can just load garnet-loader.lisp.
;;
;; To compile your library files, start up MCL and load this file.  As each
;; library file is compiled and loaded, its name will be printed in the lisp
;; buffer.  When the load of this file is finished and the MCL prompt appears,
;; quit MCL and restart it, then load Garnet (using the garnet-loader).

(let ((*record-source-file* T))

  (dolist (pair `(("ccl:library;traps" traps)
                  ("ccl:library;interfaces" interfaces)
                  ("ccl:library;quickdraw" quickdraw)))
    (compile-file (first pair))
    (print (require (second pair))))

  (dolist (pair `(("ccl:interfaces;types" types)
                  ("ccl:interfaces;quickdraw" quickdraw)
                  ("ccl:interfaces;events" events)
                  ("ccl:interfaces;osutils" osutils)
                  ("ccl:interfaces;osevents" osevents)
                  ("ccl:interfaces;memory" memory)))
    (compile-file (first pair))
    (print (ccl::require-interface (second pair))))

  (dolist (pair `(("ccl:library;lispequ" lispequ)
                  ("ccl:library;loop" loop)))
    (compile-file (first pair))
    (print (require (second pair))))

  (dolist (pair `(("ccl:interfaces;QDOffscreen" QDOffscreen)
                  ("ccl:interfaces;controls" controls)
                  ("ccl:interfaces;windows" windows)
                  ("ccl:interfaces;picker" picker)))
    (compile-file (first pair))
    (print (ccl::require-interface (second pair))))

  )