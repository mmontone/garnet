;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; This file loads all the garnet modules.
;;; 
;;; ** To prevent certain parts from being loaded, first set
;;;      user::load-XX-p to NIL.
;;; ** To get some of the parts which are not loaded by default to be loaded,
;;;    set user::load-XX-p to T.
;;; ** If you are a non-CMU user, set Your-Garnet-Pathname to be your local
;;;    Garnet directory, and set Your-CLX-Pathname to be your local CLX
;;;    directory.
;;; ** To override where something is loaded from, set Garnet-xx-PathName
;;;    before loading this file and/or Garnet-xx-src
;;;
;;; The controlling variables are:
;;; 
;;;      load-clx-p          (Default: NIL => clx not loaded)
;;;      load-utils-p        (Default: T   => utilities loaded)
;;;      load-kr-p           (Default: T   => kr loaded)
;;;      load-gworld-p       (Default: T   => gworld loaded for Mac)
;;;      load-gem-p          (Default: T   => gem loaded)
;;;      load-opal-p         (Default: T   => opal loaded)
;;;      load-inter-p        (Default: T   => interactors loaded)
;;;      load-multifont-p    (Default: NIL => multifont *NOT* loaded)
;;;      load-gesture-p      (Default: NIL => gestures *NOT* loaded)
;;;      load-ps-p           (Default: T   => ps loaded)
;;;      load-aggregadgets-p (Default: T   => aggregadgets loaded)
;;;      load-aggregraphs-p  (Default: NIL => aggregraphs *NOT* loaded)
;;;      load-gadgets-p      (Default: NIL => gadgets *NOT* loaded)
;;;      load-debug-p        (Default: T   => debugging tools loaded)
;;;      load-demos-p        (Default: NIL => demos *NOT* loaded)
;;;      load-c32-p          (Default: NIL => C32 *NOT* loaded)
;;;      load-gilt-p         (Default: NIL => gilt *NOT* loaded)
;;;      load-lapidary-p     (Default: NIL => lapidary *NOT* loaded)
;;;
;;; The first part of this file lists the file names where the various
;;; parts of Garnet come from.  This will need to be modified for each new
;;; installation of Garnet.
;;;
;;; To override any particular file name place, it is only necessary to
;;; assign the variable name Garnet-XX-Pathname before this file is loaded
;;; (since they are defined here using defvar, the old name will stay in
;;; affect).
;;;



#|
============================================================
Change log:
08/20/98 Fred Gilham    - Auto-detect CMUCL binary name.  Make
                          :external the default for garnet-version.
01/30/95 Andrew Mickish - New redefinitions of :CL and :CL-USER for CMUCL
01/05/95 Andrew Mickish - Added :CL-USER nickname in defpackage
                          redefinition of :COMMON-LISP-USER package
                          and reordered :USE arguments by suggestion
03/17/94 Andrew Mickish - Added Gworld for Mac
12/04/93 Andrew Mickish - Added Mac switches
11/01/93 Andrew Mickish - Added GEM
 9/22/93 Bruno Haible   - Added FLET for merge-pathnames in CLISP
 8/13/93 Andrew Mickish - Added user::Garnet-Readtable
 8/12/93 Andrew Mickish - Closed display in Verify-Display-Can-Be-Opened;
                          added #+garnet-processes to *features* list
 5/17/93 Andrew Mickish - Added compiler optimization proclamation
 5/13/93 Andrew Mickish - Removed commas from Garnet-Load-Alist so it
                          notices changes in the values of the pathname variables
 4/15/93 Andrew Mickish - Added lucid memory-management instruction
 4/ 5/93 Dave Kosbie    - Added Garnet-Utils package (where
                          Garnet-independent Lisp utilities will now reside)
 3/25/93 Andrew Mickish - Made Garnet-Load use an association list
 3/17/93 Andrew Mickish - Removed Motif-Gilt-Loader
10/23/92 Dave Kosbie    - Added KATIE package
08/17/92 Andrew Mickish - Added display check, changed names of switches,
                          changed names of directories, changed method
                          for determining Garnet pathnames
07/29/92 Andrew Mickish - :cmu-sparc now loads from cmu-bin, removed :cmu
                          and :test versions.
07/23/92 Dario Giuse    - moved loading of C32 before Lapidary, which needs it.
05/27/92 Joly           - Interactors package should use kr package.
05/25/92 Joly/Pervin    - Package CLTL1 does not exist in LispWorks.
05/21/92 Dario Giuse    - Added load-c32-p.
05/14/92 Szekely/Pervin - Do not launch process if compiling.
05/04/92 Russell Almond - Added allegro-v4.1 switches.
04/22/92 Ed Pervin      - Added launch-process-p switch.
04/10/92 Andrew Mickish - Added "gg:" prefix to garnet-load and garnet-compile
04/02/92 Rich McDaniel  - Added load-multifont-p
03/13/92 Ed Pervin      - Added :cmu-test
03/11/92 Andrew Mickish - Removed unlesses from CMU ext:search-list setfs
03/06/92 Andrew Mickish - Added *compiler-extension* switches
02/20/92 Andrew Mickish - Added package definitions for Explorer lisp;
                          Added gesture pathnames, etc.
02/11/92 Andrew Mickish - Added :garnet-debug to *features* list;  changed
                          pathnames from /afs/cs/ to /afs/cs.cmu.edu/.
04/25/91 Ed Pervin      - Official release of version 1.4; alpha directory
                          changed back to test.  No longer support
                          :cmu-lucid3.1 and :cmu-lucid4.0.
04/19/91 Ed Pervin      - Added lispworks to switches.
04/15/91 Ed Pervin      - Changed (make-packages **) to
                          (unless (find-package **) (make-package **)).
04/03/91 Ed Pervin      - Changed :sparc-test4.0 --> :sparc-test and
                          added :pmax-test.
03/21/91 Ed Pervin      - Release 1.4; test directory changed to alpha.
03/07/91 Andrew Mickish - Added aggregraphs.
03/07/91 Brad Myers     - Made new motif-gilt-loader, and also garnet-load.
03/01/91 Ed Pervin      - Added :sparc-test for version compiled in Allegro 4.0.
02/27/91 Dilip D'Souza  - Added everything with #+allegro-v4.0 switches.
02/25/91 Ed Pervin      - Pushed :garnet on *features* list.
01/24/91 Andrew Mickish - Added Gilt.
01/02/90 Andrew Mickish - Added :rt-test and :sparc-test options.
11/29/90 Brad Myers     - Added :cmu-sparc option.
10/05/90 Ed Pervin      - New variables Your-Garnet-Pathname and Your-CLX-Pathname
                          which determine all the :external pathnames.
08/09/90 Ed Pervin      - Release 1.3
08/07/90 Ed Pervin      - rbd --> ecp
07/25/90 Ed Pervin      - Added *dont-load-modules-twice*;  amickish --> preddy
04/02/90 Ed Pervin      - Call xlib:load-clx in Lucid explicitly.
03/19/90 Ed Pervin      - Got rid of Garnet-Font-Pathname
02/14/90 Ed Pervin      - Added color screen option
01/04/90 Ed Pervin      - Added :external option and version number
12/19/89 Ed Pervin      - Now loads CLX.
12/13/89 Ed Pervin      - Added :cmu-allegro option.
12/05/89 Brad Myers     - Fixed so works with garnet-compiler
10/30/89 Brad Myers     - New file structure and src directories;  changed
                          dont-load-xx to load-xxx-p
10/17/89 Brad Myers     - Added debug
08/18/89 Brad Myers     - Added Toolkit
06/07/89 Brad Myers     - Created
============================================================
|#


;; Lucid is behind the times in naming their "LISP" and "USER" packages.
;; All other lisps have the packages :COMMON-LISP and :COMMON-LISP-USER,
;; and Garnet just gives these the nicknames :LISP and :USER, respectively.
;; For Lucid, though, we have to do things backwards.  This difference comes
;; up again below during the DEFPACKAGE of :COMMON-LISP and :COMMON-LISP-USER.
;;
#+lucid (in-package :USER)
#+lucid (rename-package (find-package :LISP) :LISP (list :COMMON-LISP))
#+lucid (rename-package (find-package :USER) :USER (list :COMMON-LISP-USER
							 :CL-USER))

#-lucid
(in-package :COMMON-LISP-USER)

(defparameter Garnet-Version-Number "3.0")
(pushnew :GARNET *features*)
(pushnew :GARNET-V3.0 *features*)

;;; The :GARNET-DEBUG option allows many different kinds of run-time checking,
;;; and also loads some extra test code.  After you have debugged your code
;;; and want it to run faster, remove :GARNET-DEBUG from the *features* list
;;; and RECOMPILE all of Garnet and your code.  The result will be smaller and
;;; somewhat faster.
;;; To remove :GARNET-DEBUG from the *features* list, either defvar
;;; Garnet-Garnet-Debug to NIL before you load the garnet-loader, or simply
;;; comment out the next few lines.
(defvar Garnet-Garnet-Debug T)
(if Garnet-Garnet-Debug
    (pushnew :garnet-debug *features*)
    (setf *features* (delete :garnet-debug *features*)))

;;; The :GARNET-PROCESSES keyword goes on the *features* list if this version
;;; of lisp supports multiple processes.  Then things like the animation
;;; interactor can use the #+garnet-processes switch, instead of referring
;;; explicitly to different versions of lisp.
#+(or allegro lucid lispworks (and cmu mp))
(pushnew :GARNET-PROCESSES *features*)

;;; The :GARNET-BINS option controls whether Garnet uses its own constructed
;;; hash tables called "bins" or uses the system's hash tables at the kernel
;;; of the KR system.  Push :GARNET-BINS onto the *features* list for lisp
;;; implementations that compile to machine code and have slow hash tables.
;;; Don't push it for implementations like CLISP which have fast hash tables.
#-CLISP
(pushnew :GARNET-BINS *features*)


;; This variable is used by Allegro to restore the old value of the *readtable*
;; when a saved image is restarted (see opal:make-image in opal/utils.lisp).
(defvar Garnet-Readtable *readtable*)


;; Set compiler optimization settings
;;
(defvar *default-garnet-proclaim*
  #+(or allegro lispworks apple) '(optimize (speed 3) (safety 1) (space 0)
                                   (debug #+garnet-debug 3 #-garnet-debug 0))
  ;; Lucid needs a safety of 1 and compilation-speed of 0 to avoid problems
  ;; with CLX calls.
  #+lucid '(optimize (compilation-speed 0) (safety 1) (speed 2))
  #+cmu '(optimize (speed 3) (safety 1) (space 0))
  #-(or allegro lucid cmu lispworks apple) NIL)

(when *default-garnet-proclaim*
  (proclaim *default-garnet-proclaim*))


;; The function PROVIDE is no longer part of common lisp and so was
;; removed from the lisp package in the most recent release of
;; LispWorks (version 2.1 onwards). This will retrieve its definition.
#+lispworks (setf (symbol-function 'lisp::provide)
		  (symbol-function 'system::provide))
#+lispworks (export 'provide 'lisp)

#+lucid
(change-memory-management :growth-limit (max 655
					     lucid::*external-growth-limit*)
			  :expand 400)

(progn
  (defpackage :GARNET-UTILS (:use :COMMON-LISP) (:nicknames :GU))
  (defpackage :KR-DEBUG (:use :COMMON-LISP))
  (defpackage :KR (:use :COMMON-LISP :KR-DEBUG))
  (defpackage :GEM (:use :COMMON-LISP :KR :KR-DEBUG))
  (defpackage :OPAL (:use :COMMON-LISP :KR))
  (defpackage :INTERACTORS (:use :COMMON-LISP :KR) (:nicknames :INTER)
    (:export *GARNET-BREAK-KEY* *LEFT-BUTTON* *TRANS-FROM-FILE*))
  (defpackage :GARNET-GADGETS (:use :COMMON-LISP :KR) (:nicknames :GG))
  (defpackage :GARNET-DEBUG (:use :COMMON-LISP :KR :OPAL) (:nicknames :GD))
  (defpackage :GILT (:use :COMMON-LISP :KR))
  (defpackage :C32 (:use :COMMON-LISP :KR))
  (defpackage :LAPIDARY (:use :COMMON-LISP :KR))
  (defpackage :AGATE (:use :COMMON-LISP :KR))

  (defpackage :DEMO-3D (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
  (defpackage :DEMO-MULTIWIN (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
  (defpackage :DEMO-MULTIFONT (:use :COMMON-LISP KR) (:export DO-GO DO-STOP))
  (defpackage :DEMO-ANIMATOR (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
  (defpackage :DEMO-ANGLE (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
  (defpackage :DEMO-OTHELLO (:use :KR :COMMON-LISP) (:nicknames :DOTH)
    (:export DO-GO DO-STOP START-GAME STOP-GAME SET-SCORE))
  (defpackage :DEMO-PIXMAP (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
  (defpackage :DEMO-ARITH (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
  (defpackage :DEMO-SCHEMA-BROWSER (:use :COMMON-LISP :KR)
    (:export DO-GO DO-STOP SCHEMA-BROWSER SCHEMA-BROWSER-WIN
	     SCHEMA-BROWSER-TOP-AGG))
  (defpackage :DEMO-ARRAY (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
  (defpackage :DEMO-SCROLLBAR (:use :COMMON-LISP :KR)
    (:export DO-GO DO-STOP
	     MAC-obj MAC-Go MAC-Stop
	     Open-obj Open-Go Open-Stop
	     NEXT-obj NEXT-Go NEXT-Stop
	     Motif-obj Motif-Go Motif-Stop))
  (defpackage :DEMO-CLOCK (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
  (defpackage :DEMO-SEQUENCE (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
  (defpackage :DEMO-EDITOR (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
  (defpackage :DEMO-TEXT (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
  (defpackage :DEMO-FILE-BROWSER (:use :COMMON-LISP :KR)
    (:export DO-GO DO-STOP FILE-BROWSER FILE-BROWSER-WIN
	     FILE-BROWSER-TOP-AGG))
  (defpackage :DEMO-TRUCK (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
  (defpackage :DEMO-GADGETS (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
  (defpackage :DEMO-TWOP (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
  (defpackage :DEMO-GESTURE (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
  (defpackage :DEMO-UNISTROKES (:use :COMMON-LISP :KR :INTER) (:export DO-GO DO-STOP))
  (defpackage :DEMO-GRAPH (:use :COMMON-LISP :KR)
    (:export DO-GO DO-STOP SCHEMA-GRAPH DEMO-GRAPH-ERROR-GADGET ROOT-BOX
	     RELAYOUT DEMO-GRAPH-WIN))
  (defpackage :DEMO-VIRTUAL-AGG (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
  (defpackage :DEMO-GROW (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
  (defpackage :DEMO-XASPERATE (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
  (defpackage :DEMO-LOGO (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP RE-ANIMATE))
  (defpackage :DEMOS-CONTROLLER (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
  (defpackage :DEMO-MANYOBJS (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
  (defpackage :DEMO-MENU (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
  (defpackage :GARNET-CALCULATOR (:use :COMMON-LISP :KR)
    (:export START-CALC STOP-CALC DO-GO DO-STOP))
  (defpackage :DEMO-MODE (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
  (defpackage :GARNETDRAW (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
  (defpackage :DEMO-MOTIF (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
  (defpackage :MGE (:use :COMMON-LISP :KR)
    (:export DO-GO DO-STOP
	     CREATE-PIECE DESTROY-PIECE DESTROY-ALL-PIECES
	     GO-INITIALIZE EDITOR-SHOW-WINDOW))
  (defpackage :DEMO-MOVELINE (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
  )

;; Note: Mac Lisp requires that you repeat the full package definition
(defpackage :COMMON-LISP-USER (:use :KR :KR-DEBUG :GARNET-DEBUG
				    :COMMON-LISP #+apple :CCL
				    #+cmu :EXTENSIONS)
  #-(or lucid cmu)
  (:nicknames :CL-USER :USER))
#-(or lucid cmu)
(defpackage :COMMON-LISP (:nicknames :CL :LISP))

;;; *dont-load-modules-twice* tells whether to re-load modules
;;; if a user loads garnet-loader.lisp a second time.
(defparameter *dont-load-modules-twice* t)

(unless (boundp '*Garnet-Going-To-Compile*)
  (defvar load-utils-p T)
  (defvar load-kr-p T)
  (defvar load-gworld-p T)
  (defvar load-gem-p T)
  (defvar load-opal-p T)
  (defvar load-inter-p T)
  (defvar load-multifont-p NIL)
  (defvar load-gesture-p NIL)
  (defvar load-ps-p T)
  (defvar load-aggregadgets-p T)
  (defvar load-aggregraphs-p NIL)
  (defvar load-debug-p #+garnet-debug T #-garnet-debug NIL)
  (defvar load-gadgets-p NIL)
  (defvar load-demos-p NIL)
  (defvar load-lapidary-p NIL)
  (defvar load-gilt-p NIL)
  (defvar load-c32-p NIL))

;;; load-XX-p control whether the various parts are loaded or not
;;; Because these use defvar, if they are set before this file is
;;; loaded, their original value will be used.

(defparameter load-clx-p #+clx NIL #-clx T)

;;; launch-process-p controls whether Garnet will launch
;;; a separate process to detect keyboard and mouse events.
(defvar launch-process-p T)

;;; update-locking-p controls whether process locks will be activated
;;; around the update method (this keeps two processes from calling update
;;; at the same time).
(defvar update-locking-p T
  "If T, uses process locks to keep Update in a process from interrupting
   itself in a different process.")

(defun Version-Error ()
  (error "Could not determine which compiled binaries are appropriate to
load into your lisp.  Please set common-lisp-user::Garnet-Version before loading
Garnet-Loader again."))

(defun Get-Garnet-Version ()
  #+sparc    (or #+allegro-v4.0 :sparc-allegro
                 #+allegro-v4.1 :sparc-allegro4.1
                 #+allegro-v4.2 :sparc-allegro4.2
                 #+cmu     :sparc-cmucl
                 #+lucid   :sparc-lucid
                 #-(and allegro-v4.0 allegro-v4.1 allegro-v4.2 cmu lucid)
		   (version-error))
  #+dec3100  (or #+allegro-v3.1 :pmax-allegro
                 #+allegro-v4.1 :pmax-allegro4.1
                 #-(and allegro-v3.1 allegro-v4.1) (version-error))
  #+(or pa hpux) (or #+lcl4.0.3      :hp-lucid4.0.3
                     #+lucid         :hp-lucid
                     #+allegro-v4.2  :hp-allegro4.2
                     #-(or lucid allegro-v4.2)  (version-error))
  #+clisp     :clisp
  #+lispworks :alpha-lw
  #+apple     :mac
  #-(or sparc dec3100 pa hpux clisp lispworks apple) (version-error))

;;; Garnet-Version controls where the files are loaded from.
;;; Because this is a defvar, if Garnet-Version is set before this file is
;;; loaded, its original value will be used.

;;; Garnet-Version should be set to :external for non-CMU users, and
;;; Your-Garnet-Pathname should be set appropriately.
;;;
(defvar garnet-version :external)  ; was (Get-Garnet-Version)

(format T "~&** Loading Garnet Version ~a from ~s~%" Garnet-Version-Number Garnet-Version)


;;; Insert your pathname of Garnet into Your-Garnet-Pathname and where
;;; your CLX comes from into Your-CLX-pathname.  All the :external pathnames
;;; will depend on these two pathnames.
;;;
;;; On CMU's Andrew system, do
;;; (setf Your-CLX-Pathname "/usr/local/lib/cl/lib/code/")
;;; before loading garnet-loader.lisp.

(defvar Your-CLX-Pathname
  (if (eq garnet-version :external)
      "**FILL THIS IN**"                ;; SET THIS
    
      ;; Values useful at CMU:
      #+(or pa hpux) (or #+lucid "/afs/cs.cmu.edu/hp700_ux90/omega/usr/local/depot/lucid/non-kanji/"
			 #+allegro "/afs/cs/misc/allegro/hp700_ux80/beta/lib/code/")
      #+dec3100 "/usr/local/lib/cl/lib/code/"
      #+sunos4  "/usr/local/lib/cl/code/"
      #-(or pa hpux dec3100 sunos4) "/usr/misc/.allegro/lib/code/"))

(defvar Your-Garnet-Pathname
  (if (eq garnet-version :external)
      "**FILL THIS IN**"                ;; SET THIS

      ;; Values useful at CMU:
      #-apple "/afs/cs.cmu.edu/project/garnet/"
      #+apple "Macintosh HD:Garnet:"))


;; This function is required for KCL and MCL because they do not properly
;; concatenate directory pathnames
;;
(defun Garnet-Pathnames (subdir dir)
  #+(or KCL APPLE)
  (pathname (concatenate 'string (namestring dir) (namestring subdir)))
  #-(or KCL APPLE)
  (merge-pathnames subdir dir)
  )

(defun Get-Garnet-Binary-Pathname (version)
  (let ((directory-name
	 (case version
	   (:external #-apple "bin/" #+apple "src:")
	   (:sparc-allegro "sparc-allegro-bin/")
	   (:sparc-allegro4.1 "sparc-allegro4.1-bin/")
	   (:sparc-allegro4.2 "sparc-allegro4.2-bin/")
	   (:sparc-cmucl "sparc-cmucl-bin/")
	   (:sparc-lucid "sparc-lucid-bin/")
	   (:pmax-allegro "pmax-allegro-bin/")
	   (:pmax-allegro4.1 "pmax-allegro4.1-bin/")
	   (:hp-allegro4.2 "hp-allegro4.2-bin/")
	   (:hp-lucid "hp-lucid-bin/")
	   (:hp-lucid4.0.3 "hp-lucid403-bin/")
	   (:clisp "clisp-bin")
	   (:alpha-lw "alpha-lw-bin/")
	   (:mac "src:")
	   (t (error "~S is an invalid garnet-version" version)))))
    (garnet-pathnames directory-name Your-Garnet-Pathname)))


(defvar Garnet-Src-Pathname
  (garnet-pathnames #-apple "src/" #+apple "src:" Your-Garnet-Pathname))
(defvar Garnet-Binary-Pathname (Get-Garnet-Binary-Pathname garnet-version))
(defvar Garnet-Lib-Pathname
  (garnet-pathnames #-apple "lib/" #+apple "lib:" Your-Garnet-Pathname))
(defvar CLX-Pathname Your-CLX-Pathname)

(defvar Garnet-Utils-Src
  (garnet-pathnames #-apple "utils/" #+apple "utils:" Garnet-Src-Pathname))
(defvar Garnet-Utils-Pathname
  (garnet-pathnames #-apple "utils/" #+apple "utils:" Garnet-Binary-Pathname))
(defvar Garnet-KR-Src
  (garnet-pathnames #-apple "kr/" #+apple "kr:" Garnet-Src-Pathname))
(defvar Garnet-KR-Pathname
  (garnet-pathnames #-apple "kr/" #+apple "kr:" Garnet-Binary-Pathname))
(defvar Garnet-Gworld-Src
  (garnet-pathnames #-apple "gworld/" #+apple "gworld:" Garnet-Src-Pathname))
(defvar Garnet-Gworld-Pathname
  (garnet-pathnames #-apple "gworld/" #+apple "gworld:" Garnet-Binary-Pathname))
(defvar Garnet-Gem-Src
  (garnet-pathnames #-apple "gem/" #+apple "gem:" Garnet-Src-Pathname))
(defvar Garnet-Gem-Pathname
  (garnet-pathnames #-apple "gem/" #+apple "gem:" Garnet-Binary-Pathname))
(defvar Garnet-Opal-Src
  (garnet-pathnames #-apple "opal/" #+apple "opal:" Garnet-Src-Pathname))
(defvar Garnet-Opal-Pathname
  (garnet-pathnames #-apple "opal/" #+apple "opal:" Garnet-Binary-Pathname))
(defvar Garnet-Inter-Src
  (garnet-pathnames #-apple "inter/" #+apple "inter:" Garnet-Src-Pathname))
(defvar Garnet-Inter-Pathname
  (garnet-pathnames #-apple "inter/" #+apple "inter:" Garnet-Binary-Pathname))
(defvar Garnet-Gesture-Src
  (garnet-pathnames #-apple "gesture/" #+apple "gesture:" Garnet-Src-Pathname))
(defvar Garnet-Gesture-Pathname
  (garnet-pathnames #-apple "gesture/" #+apple "gesture:" Garnet-Binary-Pathname))
(defvar Garnet-Aggregadgets-Src
  (garnet-pathnames #-apple "aggregadgets/" #+apple "aggregadgets:" Garnet-Src-Pathname))
(defvar Garnet-Aggregadgets-Pathname
  (garnet-pathnames #-apple "aggregadgets/" #+apple "aggregadgets:" Garnet-Binary-Pathname))
(defvar Garnet-PS-Src
  (garnet-pathnames #-apple "ps/" #+apple "ps:" Garnet-Src-Pathname))
(defvar Garnet-PS-Pathname
  (garnet-pathnames #-apple "ps/" #+apple "ps:" Garnet-Binary-Pathname))
(defvar Garnet-Gadgets-Src
  (garnet-pathnames #-apple "gadgets/" #+apple "gadgets:" Garnet-Src-Pathname))
(defvar Garnet-Gadgets-Pathname
  (garnet-pathnames #-apple "gadgets/" #+apple "gadgets:" Garnet-Binary-Pathname))
(defvar Garnet-Debug-Src
  (garnet-pathnames #-apple "debug/" #+apple "debug:" Garnet-Src-Pathname))
(defvar Garnet-Debug-Pathname
  (garnet-pathnames #-apple "debug/" #+apple "debug:" Garnet-Binary-Pathname))
(defvar Garnet-Demos-Src
  (garnet-pathnames #-apple "demos/" #+apple "demos:" Garnet-Src-Pathname))
(defvar Garnet-Demos-Pathname
  (garnet-pathnames #-apple "demos/" #+apple "demos:" Garnet-Binary-Pathname))
(defvar Garnet-Gilt-Src
  (garnet-pathnames #-apple "gilt/" #+apple "gilt:" Garnet-Src-Pathname))
(defvar Garnet-Gilt-Pathname
  (garnet-pathnames #-apple "gilt/" #+apple "gilt:" Garnet-Binary-Pathname))
(defvar Garnet-C32-Src
  (garnet-pathnames #-apple "c32/" #+apple "c32:" Garnet-Src-Pathname))
(defvar Garnet-C32-Pathname
  (garnet-pathnames #-apple "c32/" #+apple "c32:" Garnet-Binary-Pathname))
(defvar Garnet-Lapidary-Src
  (garnet-pathnames #-apple "lapidary/" #+apple "lapidary:" Garnet-Src-Pathname))
(defvar Garnet-Lapidary-Pathname
  (garnet-pathnames #-apple "lapidary/" #+apple "lapidary:" Garnet-Binary-Pathname))
(defvar Garnet-Contrib-Src
  (garnet-pathnames #-apple "contrib/" #+apple "contrib:" Garnet-Src-Pathname))
(defvar Garnet-Contrib-Pathname
  (garnet-pathnames #-apple "contrib/" #+apple "contrib:" Garnet-Binary-Pathname))

(defvar Garnet-Bitmap-Pathname
  (garnet-pathnames #-apple "bitmaps/" #+apple "bitmaps:" Garnet-Lib-Pathname))
(defvar Garnet-Pixmap-Pathname
  (garnet-pathnames #-apple "pixmaps/" #+apple "pixmaps:" Garnet-Lib-Pathname))
(defvar Garnet-Gilt-Bitmap-Pathname
  (garnet-pathnames #-apple "gilt/" #+apple "gilt:" Garnet-Lib-Pathname))
(defvar Garnet-C32-Bitmap-Pathname
  (garnet-pathnames #-apple "c32/" #+apple "c32:" Garnet-Lib-Pathname))
(defvar Garnet-DataFile-Pathname
  (garnet-pathnames #-apple "data/" #+apple "data:" Garnet-Lib-Pathname))
(defvar Garnet-Gesture-Data-Pathname
  (garnet-pathnames #-apple "gesture/" #+apple "gesture:" Garnet-Lib-Pathname))


;;;----------------------------------------------------------

;;; When compiling, the binaries will be in the same directories as the
;;; source files, so make all the path names be the same
;;;
;;; After compilation is finished, the user should move all the binaries
;;; into their own directories, as specified by the pathnames above.
(defvar *Garnet-Going-To-Compile*)

(when (and (boundp '*Garnet-Going-To-Compile*)
	   *Garnet-Going-To-Compile*)
  (setf Garnet-Utils-Pathname Garnet-Utils-Src)
  (setf Garnet-KR-Pathname Garnet-KR-Src)
  (setf Garnet-Gworld-Pathname Garnet-Gworld-Src)
  (setf Garnet-Gem-Pathname Garnet-Gem-Src)
  (setf Garnet-Opal-Pathname Garnet-Opal-Src)
  (setf Garnet-Inter-Pathname Garnet-Inter-Src)
  (setf Garnet-Gesture-Pathname Garnet-Gesture-Src)
  (setf Garnet-PS-Pathname Garnet-PS-Src)
  (setf Garnet-Aggregadgets-Pathname Garnet-Aggregadgets-Src)
  (setf Garnet-Gadgets-Pathname Garnet-Gadgets-Src)
  (setf Garnet-Debug-Pathname Garnet-Debug-Src)
  (setf Garnet-Demos-Pathname Garnet-Demos-Src)
  (setf Garnet-Gilt-Pathname Garnet-Gilt-Src)
  (setf Garnet-C32-Pathname Garnet-C32-Src)
  (setf Garnet-Lapidary-Pathname Garnet-Lapidary-Src)
  (setf Garnet-Contrib-Pathname Garnet-Contrib-Src)
  )

;;;----------------------------------------------------------

;;; If at cmu, then set up the search lists
#+cmu
(progn
  (setf (ext:search-list "utils:")
	(list (namestring Garnet-Utils-PathName)))
  (setf (ext:search-list "utils-src:")
	(list (namestring Garnet-Utils-Src)))

  (setf (ext:search-list "kr:")
	(list (namestring Garnet-KR-PathName)))
  (setf (ext:search-list "kr-src:")
	(list (namestring Garnet-KR-Src)))

  (setf (ext:search-list "gem:")
	(list (namestring Garnet-Gem-PathName)))
  (setf (ext:search-list "gem-src:")
	(list (namestring Garnet-Gem-Src)))

  (setf (ext:search-list "opal:")
	(list (namestring Garnet-Opal-PathName)))
  (setf (ext:search-list "opal-src:")
	(list (namestring Garnet-Opal-Src)))

  (setf (ext:search-list "inter:")
	(list (namestring Garnet-Inter-PathName)))
  (setf (ext:search-list "inter-src:")
	(list (namestring Garnet-Inter-Src)))

  (setf (ext:search-list "gesture:")
	(list (namestring Garnet-Gesture-PathName)))
  (setf (ext:search-list "gesture-src:")
	(list (namestring Garnet-Gesture-Src)))
  (setf (ext:search-list "gesture-data:")
	(list (namestring Garnet-Gesture-Data-PathName)))

  (setf (ext:search-list "ps:")
	(list (namestring Garnet-PS-PathName)))
  (setf (ext:search-list "ps-src:")
	(list (namestring Garnet-PS-Src)))

  (setf (ext:search-list "aggregadgets:")
	(list (namestring Garnet-Aggregadgets-PathName)))
  (setf (ext:search-list "aggregadgets-src:")
	(list (namestring Garnet-Aggregadgets-Src)))

  (setf (ext:search-list "gadgets:")
	(list (namestring Garnet-Gadgets-PathName)))
  (setf (ext:search-list "gadgets-src:")
	(list (namestring Garnet-Gadgets-Src)))

  (setf (ext:search-list "debug:")
	(list (namestring Garnet-Debug-PathName)))
  (setf (ext:search-list "debug-src:")
	(list (namestring Garnet-Debug-Src)))

  (setf (ext:search-list "demos:")
	(list (namestring Garnet-Demos-PathName)))
  (setf (ext:search-list "demos-src:")
	(list (namestring Garnet-Demos-Src)))

  (setf (ext:search-list "gilt:")
	(list (namestring Garnet-Gilt-PathName)))
  (setf (ext:search-list "gilt-src:")
	(list (namestring Garnet-Gilt-Src)))

  (setf (ext:search-list "c32:")
	(list (namestring Garnet-C32-PathName)))
  (setf (ext:search-list "c32-src:")
	(list (namestring Garnet-C32-Src)))

  (setf (ext:search-list "lapidary:")
	(list (namestring Garnet-Lapidary-PathName)))
  (setf (ext:search-list "lapidary-src:")
	(list (namestring Garnet-Lapidary-Src)))

  (setf (ext:search-list "contrib:")
	(list (namestring Garnet-Contrib-PathName)))
  (setf (ext:search-list "contrib-src:")
	(list (namestring Garnet-Contrib-Src)))

  )

(defparameter Garnet-Utils-Loader
  (merge-pathnames "utils-loader" Garnet-Utils-PathName))

(defparameter Garnet-KR-Loader
  (merge-pathnames "kr-loader" Garnet-KR-PathName))

(defparameter Garnet-Gworld-Loader
  (merge-pathnames "gworld-loader" Garnet-Gworld-PathName))

(defparameter Garnet-Gem-Loader
  (merge-pathnames "gem-loader" Garnet-Gem-PathName))

(defparameter Garnet-Opal-Loader
  (merge-pathnames "opal-loader" Garnet-Opal-PathName))

(defparameter Garnet-Inter-Loader
  (merge-pathnames "inter-loader" Garnet-Inter-PathName))

(defparameter Garnet-Multifont-Loader
  (merge-pathnames "multifont-loader" Garnet-Opal-PathName))

(defparameter Garnet-Gesture-Loader
  (merge-pathnames "gesture-loader" Garnet-Gesture-PathName))

(defparameter Garnet-PS-Loader
  (merge-pathnames "ps-loader" Garnet-PS-PathName))

(defparameter Garnet-Aggregadgets-Loader
  (merge-pathnames "aggregadgets-loader" Garnet-Aggregadgets-PathName))

(defparameter Garnet-Aggregraphs-Loader
  (merge-pathnames "aggregraphs-loader" Garnet-Aggregadgets-PathName))

(defparameter Garnet-Gadgets-Loader
  (merge-pathnames "gadgets-loader" Garnet-Gadgets-PathName))

(defparameter Garnet-Debug-Loader
  (merge-pathnames "debug-loader" Garnet-Debug-PathName))

(defparameter Garnet-Demos-Loader
  (merge-pathnames "demos-loader" Garnet-Demos-PathName))

(defparameter Garnet-Gilt-Loader
  (merge-pathnames "gilt-loader" Garnet-Gilt-PathName))

(defparameter Garnet-C32-Loader
  (merge-pathnames "c32-loader" Garnet-C32-PathName))

(defparameter Garnet-Lapidary-Loader
  (merge-pathnames "lapidary-loader" Garnet-Lapidary-PathName))


;--------------------------------------------------------------------

(defparameter Garnet-Load-Alist
  `(("gg" . Garnet-Gadgets-PathName)
    ("gadgets" . Garnet-Gadgets-PathName)
    ("utils" . Garnet-Utils-PathName)
    ("kr" . Garnet-KR-PathName)
    ("gworld" . Garnet-Gworld-Pathname)
    ("gem" . Garnet-Gem-Pathname)
    ("opal" . Garnet-Opal-Pathname)
    ("inter" . Garnet-Inter-PathName)
    ("gesture" . Garnet-Gesture-PathName)
    ("gestures" . Garnet-Gesture-PathName)
    ("ps" . Garnet-PS-PathName)
    ("aggregadgets" . Garnet-Aggregadgets-PathName)
    ("debug" . Garnet-Debug-PathName)
    ("demos" . Garnet-Demos-PathName)
    ("demo" . Garnet-Demos-PathName)
    ("gilt" . Garnet-Gilt-PathName)
    ("c32" . Garnet-C32-PathName)
    ("lapidary" . Garnet-Lapidary-PathName)
    ("contrib" . Garnet-Contrib-PathName)
    ("utils-src" . Garnet-Utils-Src)
    ("kr-src" . Garnet-KR-Src)
    ("gworld-src" . Garnet-Gworld-Src)
    ("gem-src" . Garnet-Gem-Src)
    ("opal-src" . Garnet-Opal-Src)
    ("inter-src" . Garnet-Inter-Src)
    ("gesture-src" . Garnet-Gesture-Src)
    ("gestures-src" . Garnet-Gesture-Src)
    ("ps-src" . Garnet-PS-Src)
    ("aggregadgets-src" . Garnet-Aggregadgets-Src)
    ("gadgets-src" . Garnet-Gadgets-Src)
    ("gg-src" . Garnet-Gadgets-Src)
    ("debug-src" . Garnet-Debug-Src)
    ("demos-src" . Garnet-Demos-Src)
    ("demo-src" . Garnet-Demos-Src)
    ("gilt-src" . Garnet-Gilt-Src)
    ("c32-src" . Garnet-C32-Src)
    ("lapidary-src" . Garnet-Lapidary-Src)
    ("contrib-src" . Garnet-Contrib-Src)
    ("clx" . CLX-PathName)
    ))

(defun Add-Garnet-Load-Prefix (prefix pathname)
  (push (cons prefix pathname) Garnet-Load-Alist))

#+apple
(defun Truncate-FileName (name)
  (let ((pos (position #\. name)))
    (if pos
        ;; Assume suffix is supplied
        name
        ;; Else, add suffix for trouble files
        (if (>= (length name) 27)
            (cond
             ((string= name "motif-prop-sheet-win-loader")
              "motif-prop-sheet-win-loader.lis")
             ((string= name "motif-scrolling-menu-loader")
              "motif-scrolling-menu-loader.lis")
             ((string= name "motif-scrolling-window-loader")
              "motif-scrolling-window-loader.l")
             ((string= name "motif-scrolling-labeled-box-loader")
              "motif-scrolling-labeled-box-loa")
             ((string= name "motif-scrolling-labeled-box")
              "motif-scrolling-labeled-box.fas")
             ((string= name "scrolling-labeled-box-loader")
              "scrolling-labeled-box-loader.li")
             ((string= name "scrolling-input-string-loader")
              "scrolling-input-string-loader.l")
             (t (error "Truncate-Filename can't map ~S
to a 31 character filename with a .lisp suffix."
                       name)))
            ;; Not a trouble file
            name))))


;; On newer versions of clisp, this makes the renaming .lisp -> .lsp obsolete.
#+clisp
(when (boundp 'system::*source-file-types*)
  (pushnew (pathname ".lisp") system::*source-file-types*)
)


(defun Garnet-Load (filename)
  (let ((pos (position #\: filename)))
    (if pos
	(let* ((head (subseq filename 0 pos))
	       (tail (subseq filename (1+ pos)))
	       (prefix (or (eval (cdr (assoc head Garnet-Load-Alist
					     :test #'string=)))
			   (error "Bad prefix ~S~%" head)))
	       (finalname #+apple (garnet-pathnames
                                   (truncate-filename tail) prefix)
                          #-apple (garnet-pathnames tail prefix)))
	  (format T "Loading ~s~%" finalname)
	  (load finalname))
	;; else no colon, load regular
	(progn
	  (format T "NO COLON, Loading ~s~%" filename)
	  (load filename)))))

;;; 
;;; This function will compile your garnet files while keeping the
;;; sources and binaries separated.  If you want to just compile one
;;; file from Garnet, like the gadget file gauge.lisp, then you could
;;; use this function to compile the source file and automatically
;;; save the binary file in the bin directory.
;;;
;;; Example:
;;;    (garnet-compile "gadgets:gauge") 
;;;    Takes the source file from Garnet-Gadgets-Src, compiles it, and
;;;    saves the binary file in Garnet-Gadgets-Pathname (the binary
;;;    gadgets directory).
;;;
(defvar *compiler-extension*
  #+allegro ".fasl"
  #+(and lucid sparc)     ".sbin"
  #+(and lucid pa)        ".hbin"
  #+cmu                   (concatenate 'string "." (c:backend-fasl-file-type c:*backend*))
;;;  #+(and cmu sparc)       ".sparcf"
;;;  #+(and cmu pa)          ".hpf"
;;;  #+(and cmu (not sparc)
;;;	     (not pa))    ".fasl"
  #+lispworks             ".afasl"
  #+apple                 ".fasl"
  #+clisp                (if (boundp 'system::*compiled-file-types*)
			     (namestring (first system::*compiled-file-types*))
			     ".fas"))


(defun Garnet-Compile (filename)
  (let ((pos (position #\: filename)))
    (if pos
	(let* ((head (subseq filename 0 pos))
	       (tail (subseq filename (1+ pos)))
	       (head-src (concatenate 'string head "-src"))
	       (src-prefix
		(or (eval (cdr (assoc head-src Garnet-Load-Alist
				      :test #'string=)))
		    (eval (cdr (assoc head Garnet-Load-Alist
				      :test #'string=)))
		    (error "Prefix ~S not found in Garnet-Load-Alist"
			   head)))
	       (bin-prefix
		(or (eval (cdr (assoc head Garnet-Load-Alist
				      :test #'string=)))
		    (error "Prefix ~S not found in Garnet-Load-Alist"
			   head)))
               (src-finalname (garnet-pathnames
                               (concatenate 'string tail ".lisp") src-prefix))
               (bin-finalname (garnet-pathnames
                               (concatenate 'string tail *compiler-extension*)
                               bin-prefix)))
               
          #+apple
          ;; Oops, might need to recalculate src-filename and bin-filename
          ;; if filenames are longer than 31 characters
          (when (>= (length tail) 27)
            (let* ((finalname-aux (truncate-filename tail))
                   (finalname (subseq finalname-aux 0
                                      (position #\. finalname-aux))))
              (setf src-finalname
                    (garnet-pathnames
                     (subseq (concatenate 'string finalname ".lisp") 0 31)
                     src-prefix))
              (setf bin-finalname
                    (garnet-pathnames
                     (subseq (concatenate 'string finalname
                                          *compiler-extension*) 0 31)
                     bin-prefix))))
                
          (format T "Compiling ~s~%" src-finalname)
          (format T "for output to ~s~%" bin-finalname)
          (compile-file src-finalname :output-file bin-finalname))
        ;; else no colon, abort
        (error "NO COLON, aborting compile"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;
;;; The real load
;;;

(format t "...Loading Garnet ...~%")
(setf *load-verbose* t)

(cond
  (load-clx-p
   (defparameter CLX-Loader
     #+lucid (merge-pathnames "windows" CLX-Pathname)
     #+lispworks (merge-pathnames "defsys" CLX-Pathname)
     #-(or lucid lispworks) (merge-pathnames "clx" CLX-Pathname))
   (format T "~% %%%%%%% Loading ~A %%%%%%%%~%" #-apple "CLX"
	                                        #+apple "MCL Libraries")
   #+apple
   (progn (require 'traps)
          (require 'interfaces)
          (require 'quickdraw)
          ;; For event-handling
          (ccl::require-interface 'types)
          (ccl::require-interface 'quickdraw)
          (ccl::require-interface 'events)
          ;; For allowing key-up events to be perceived
          (ccl::require-interface 'OSutils)
          (ccl::require-interface 'OSevents)
          ;; For creating Pen State records (as in gestureinter.lisp)
          (ccl::require-interface 'memory)
          ;; For off-screen drawing, like double-buffering and pixmaps
          (require 'lispequ)
          (require 'loop)
          (ccl::require-interface 'QDOffscreen)
          (ccl::require-interface 'controls)
          (ccl::require-interface 'windows)
          (ccl::require-interface 'picker)
          (terpri))
   #-apple (load CLX-Loader)
   ;#+lucid (load-clx CLX-Pathname)
   )
  (t
   (format T "~%****** NOT Loading CLX *******~%")))


;; The macro DECLAIM is part of the new CLTL2 standard, and some lisps
;; (like Lucid) may not have defined it yet
;; NOTE: Have to do this after making sure CLX is loaded, since declaim needs
;; to be shadowed in the :XLIB package.
#+lucid
(unless (fboundp 'lisp::declaim)
  (defmacro lisp::declaim (lisp::arg)
    `(eval-when (eval load compile)
       (proclaim ',lisp::arg)))
  (if (and (find-package :XLIB)
           (fboundp 'xlib::declaim))
      (shadow 'lisp::declaim :XLIB))
  (export 'lisp::declaim :LISP))



;;;
;;;  Functions that will determine whether the display can be opened
;;;

#-apple
(defun get-full-display-name ()
   #+cmu (cdr (assoc :DISPLAY lisp::*environment-list*))
   #+(or allegro lispworks kcl clisp) (sys::getenv "DISPLAY")
   #+(and lucid lcl3.0) (lucid-common-lisp:environment-variable "DISPLAY")
   #+(and lucid (not lcl3.0)) (system:environment-variable "DISPLAY")
   )

#-apple
(defun get-display-name (display)
  (do* ((dlist (coerce display 'list) (cdr dlist))
        (c (car dlist) (car dlist))
        (namelist nil))
       ((or (eq c nil) (eq c '#\:)) (coerce (reverse namelist) 'string))
    (push c namelist)))

#-apple
(defun get-display-number (display)
  (let* ((dlist (coerce display 'list))
         (numstr (progn
                   (do ((c (pop dlist) (pop dlist)))
                       ((or (eq c nil) (eq c '#\:))))
                   (do ((c (pop dlist) (pop dlist))
                        (numlist nil)
                        )
                       ((or (eq c nil) (eq c '#\.))
                        (coerce (reverse numlist) 'string))
                       (push c numlist)
                       )
                   ))
         (num (if (equal numstr "") 0 (read-from-string numstr)))
         )
    num))

#-apple
(defun verify-display-can-be-opened ()
  (let* ((full-display-name (get-full-display-name))
	 (d-name (if full-display-name
		     (get-display-name full-display-name)
		     #-(or allegro clisp) (machine-instance)
		     #+clisp ""
		     #+allegro (short-site-name)))
	 (d-number (get-display-number full-display-name)))
    (multiple-value-bind (val errorp)
	#+cmu (ignore-errors (xlib:open-display d-name :display d-number))
	#+lucid (system::ignore-errors
		 (xlib:open-display d-name :display d-number))
	#+allegro (excl::ignore-errors
		   (xlib:open-display d-name :display d-number))
	#+lispworks (common-lisp:ignore-errors
		     (xlib:open-display d-name :display d-number))
	#-(or cmu lucid allegro lispworks)
	  (xlib:open-display d-name :display d-number) ; just try it
	  
    (if errorp
	(error "Could not open a display for ~S.
     You must already be running X to load or compile Garnet.  Your DISPLAY
environment variable must be set with the name of the machine on which the
Garnet windows will be displayed.  Please exit lisp and execute a command
like the following to the unix shell before loading or compiling Garnet:
  \"setenv DISPLAY windowmachine.cs.cmu.edu:0.0\"
  \"setenv DISPLAY unix:0.0\"
  \"setenv DISPLAY 0.0\"
The last two values may be more efficient when you want the Garnet windows
to appear on the same machine that Garnet is running on.
     Additionally, you must execute the command \"xhost +\" on the machine
that the windows will be displayed on, if it is different from the machine
running Garnet."
	       full-display-name))
    (xlib:close-display val)
    T)))

#-apple
(verify-display-can-be-opened)

;;;
;;; Now back to loading Garnet
;;;

(if load-utils-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :utils))
	(format T "~%****** Utils already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Utils %%%%%%%%~%")
          (load Garnet-Utils-Loader)))
    (format T "~%****** NOT Loading Utils *******~%"))

(if load-kr-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :kr))
	(format T "~%****** KR already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading KR %%%%%%%%~%")
          (load Garnet-KR-Loader)))
    (format T "~%****** NOT Loading KR *******~%"))

#+apple
(if load-gworld-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :gworld))
	(format T "~%****** Gworld already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Gworld %%%%%%%%~%")
          (load Garnet-Gworld-loader)))
    (format T "~%****** NOT Loading Gworld *******~%"))

(if load-gem-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :gem))
	(format T "~%****** Gem already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Gem %%%%%%%%~%")
          (load Garnet-Gem-Loader)))
    (format T "~%****** NOT Loading Gem *******~%"))

(if load-opal-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :opal))
	(format T "~%****** Opal already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Opal %%%%%%%%~%")
          (load Garnet-Opal-Loader)))
    (format T "~%****** NOT Loading Opal *******~%"))

(if load-inter-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :inter))
	(format T "~%****** Interactors already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Interactors %%%%%%%%~%")
          (load Garnet-Inter-Loader)))
    (format T "~%****** NOT Loading Interactors *******~%"))

(if load-multifont-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :multifont))
	(format T "~%****** Multifont already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Multifont %%%%%%%%~%")
          (load Garnet-Multifont-Loader)))
    (format T "~%****** NOT Loading Multifont *******~%"))

(if load-gesture-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :gesture))
	(format T "~%****** Gestures already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Gestures %%%%%%%%~%")
          (load Garnet-Gesture-Loader)))
    (format T "~%****** NOT Loading Gestures *******~%"))

(if load-ps-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :ps))
	(format T "~%****** PS already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading PS %%%%%%%%~%")
          (load Garnet-PS-Loader)))
    (format T "~%****** NOT Loading PS *******~%"))

(if load-aggregadgets-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :aggregadgets))
	(format T "~%****** Aggregadgets already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Aggregadgets %%%%%%%%~%")
          (load Garnet-Aggregadgets-Loader)))
    (format T "~%****** NOT Loading Aggregadgets *******~%"))

(if load-aggregraphs-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :aggregraphs))
	(format T "~%****** Aggregraphs already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Aggregraphs %%%%%%%%~%")
          (load Garnet-Aggregraphs-Loader)))
    (format T "~%****** NOT Loading Aggregraphs *******
** To load aggregraph programs, execute (load Garnet-Aggregraphs-Loader)~%"))


(if load-gadgets-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :gadgets))
	(format T "~%****** Gadgets already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Gadgets %%%%%%%%~%")
          (load Garnet-Gadgets-Loader)))
    (format T "~%****** NOT Loading Gadgets *******~%"))

(if load-debug-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :debug))
	(format T "~%****** Debugging programs already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Debugging programs %%%%%%%%~%")
          (load Garnet-Debug-Loader)))
    (format T "~%****** NOT Loading Debug Files *******
** To load debug programs, execute (load Garnet-Debug-Loader)~%"))

(if load-demos-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :demos))
	(format T "~%****** Demos already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Demos %%%%%%%%~%")
          (load Garnet-Demos-Loader)))
    (format T "~%****** NOT Loading Demos *******
** To load Demos, execute (load Garnet-Demos-Loader)~%"))

(if load-gilt-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :gilt))
	(format T "~%****** Gilt already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Gilt %%%%%%%%~%")
          (load Garnet-Gilt-Loader)))
    (format T "~%****** NOT Loading Gilt *******
** To load Gilt, execute (load Garnet-Gilt-Loader)~%"))

(if load-c32-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :c32))
	(format T "~%****** C32 already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading C32 %%%%%%%%~%")
          (load Garnet-C32-Loader)))
    (format T "~%****** NOT Loading C32 *******
** To load C32, execute (load Garnet-C32-Loader)~%"))

(if load-lapidary-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :lapidary))
	(format T "~%****** Lapidary already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Lapidary %%%%%%%%~%")
          (load Garnet-Lapidary-Loader)))
    (format T "~%****** NOT Loading Lapidary *******
** To load Lapidary, execute (load Garnet-Lapidary-Loader)~%"))


#|
;; Only set the k-reader if we are loading, not compiling
(if (get :garnet-modules :kr)
  (set-dispatch-macro-character #\# #\k (function kr::k-reader)))
|#


(format t "~%... Garnet Load Complete ...~%")

