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
;;;      load-kr-doc-p       (Default: NIL => kr-doc *NOT* loaded)
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
;;;      load-protected-eval-p (Default: <lisp dependent> =>
;;;                            protected-eval loaded for lisps with
;;;                            multi-processor support.
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
10/04/03 Russell Almond - Changed #+garnet-protected-eval to
                          (load-protected-eval-p) (Protected-eval
			  basically replaces code in processes.lisp
10/04/03 Russell Almond - Added fix for MCL #\return vs #\linefeed
                          issue. (do-load function).
10/04/03 Russell Almond - Added new extensions for MCL-5.0
10/02/03 Russell Almond - Added KR-doc flags.
10/02/03 Russell Almond - Added support for Protected-Eval
29-sep-2003 Robert Goldman - Add trial version of Allegro-specific code to open
                          display using Xauthorization information.
15-Nov-2002 Fred Gilham - Add protected-eval module (from contrib/prompter code).
                          Added #+garnet-protected-eval feature to allow process
                          code to be compiled appropriately.
08/20/98 Fred Gilham    - Auto-detect CMUCL binary name.  Make
                          :external the default for garnet-version.
???????? Russell Almond - Changed to use (require :clx) instead of
                          loading CLX explicitly.
???????? Russell Almond - Better support for multiple external
                          versions of Garnet.
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
 5/13/93 Andrew Mickish - Removed commas from Garnet-Load-Alist so it notices
                          changes in the values of the pathname variables
 4/15/93 Andrew Mickish - Added lucid memory-management instruction
 4/ 5/93 Dave Kosbie    - Added Garnet-Utils package (where Garnet-independent
                          Lisp utilities will now reside)
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

#|
This is a table of various lisp flavors which Garnet has at one time
or another been compiled on:

allegro (Allegro Common Lisp, Commercial).  Starting with version 3.0 (currently
        at 6.2).  Note that allegro has a special switch (allegro>= x
        y) which allows one to test for a specific version or later.
lucid (Lucid Common Lisp, Commerical).  Now out of business, bought by
      Allegro.
lispworks
cmu (Carnegie Mellon Lisp, Free).  Open source lisp.  Later versions
    support multiple processes (and cmu mp).
clisp (C-Lisp, Free) Lisp to C compiler.
kcl (Kyoto Common Lisp, Free).  Open source lisp.


mcl (Macintosh Common Lisp, Commercial).  Common Lisp for Macintosh.
    Version 3 and later has multiprocess support.  Note was previously
    called "Coral Common Lisp" so uses abbreviations ccl, ccl-3 &c.

apple This nominally refers to lisps running on Apple Macintosh
      computers.  Apple paid for/assisted in support for porting
      Garnet to Mac OS in early 90s.  With Mac OS X, Apple provides X
      windows support, so nominally we could run under CLX too.
      Probably many of these switches need to be changed to
      (and apple (not clx)).  I'm still experimenting with Mac OS X
      version of garnet.

Note that nobody ever paid the CMU development team for Windows port.
Contributions are welcome here.  I am also working on an OpenGL port
which may make it easier to run cross platform.

     --Russell Almond 10/02/03

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
;;; RGA -- Added cmu and MCL (version 3 plus) to this list.
#+(or allegro lucid lispworks (and cmu mp) ccl-3)
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
;; RGA --- It was then restored to CL spec.  This is probably broken.
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
                                    #+clisp :EXT
				    #+cmu :EXTENSIONS)
  #-(or lucid cmu)
  (:nicknames :CL-USER :USER))
#-(or lucid ansi-cl)
(defpackage :COMMON-LISP (:nicknames :CL :LISP))

;;; *dont-load-modules-twice* tells whether to re-load modules
;;; if a user loads garnet-loader.lisp a second time.
(defparameter *dont-load-modules-twice* t)

(unless (boundp '*Garnet-Going-To-Compile*)
  (defvar load-utils-p T)
  (defvar load-kr-p T)
  (defvar load-kr-doc-p NIL)
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
  (defvar load-protected-eval-p T)
  (defvar load-lapidary-p NIL)
  (defvar load-gilt-p NIL)
  (defvar load-c32-p NIL))

;;; load-XX-p control whether the various parts are loaded or not
;;; Because these use defvar, if they are set before this file is
;;; loaded, their original value will be used.

;;; RGA --- This means that the local lisp installation takes care of
;;; finding CLX and we don't have to (require :clx)

(defvar load-clx-p #+clx NIL #-clx T)

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

(defvar Multiple-Garnet-Bin-Dirs nil
  "Set this to T if you want the name of the bin directory to
   vary with the version of garnet, handy if you are debugging
   with multiple Lisp versions.")

(defun Get-Garnet-Version ()
  #+sparc    (or #+allegro-v4.0 :sparc-allegro
                 #+allegro-v4.1 :sparc-allegro4.1
                 #+allegro-v4.2 :sparc-allegro4.2
                 #+cmu     :sparc-cmucl
                 #+lucid   :sparc-lucid
                 #-(and allegro-v4.0 allegro-v4.1 allegro-v4.2
			cmu lucid)
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
  #+apple     (or #+clx :mac-clx
		  #+ccl-5.0 (if (osx-p) :mac-osx :mac)
		  #-(and ccl (not ccl-5.0)) :mac
		  #-ccl (version-error))
  #+macosx    (or #+allegro-v6.1 :macosx-acl-clx
		  (version-error))
  #-(or sparc dec3100 pa hpux clisp lispworks apple macosx) (version-error))

;;; Garnet-Version controls where the files are loaded from.
;;; Because this is a defvar, if Garnet-Version is set before this file is
;;; loaded, its original value will be used.

;;; Garnet-Version should be set to :external for non-CMU users, and
;;; Your-Garnet-Pathname should be set appropriately.
;;;

#+(or allegro-v6.2)(defparameter Garnet-Version :external-6.2-solaris)
#+(or allegro-v6.1)(defparameter Garnet-Version :external-6.1-solaris)
#+(or allegro-v5.0 allegro-v5.0.1)(defparameter Garnet-Version #+SUNOS4 :external-5.0-sunos
		                     #-SUNOS4 :external-5.0-solaris)
#+(or allegro-v4.2 allegro-v4.3)(defparameter Garnet-Version #+SUNOS4 :external-4.2-sunos
		                     #-SUNOS4 :external-4.2-solaris)
#+allegro-v4.1(defparameter Garnet-Version :external-4.1)
#+cmu(defparameter Garnet-Version :external-cmu)
#-(or cmu allegro)(defvar garnet-version(Get-Garnet-Version))

(format T "~&** Loading Garnet Version ~a from ~s~%" Garnet-Version-Number Garnet-Version)


;;; Insert your pathname of Garnet into Your-Garnet-Pathname and where
;;; your CLX comes from into Your-CLX-pathname.  All the :external pathnames
;;; will depend on these two pathnames.
;;;
;;; On CMU's Andrew system, do
;;; (setf Your-CLX-Pathname "/usr/local/lib/cl/lib/code/")
;;; before loading garnet-loader.lisp.

;;; RGA --- This can be made mostly obsolete by simply doing (require
;;; :clx) before loading garnet.
					;(require :clx)
;;; RGA I moved this line up earlier to make register in the defvar.

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
  (if (member garnet-version '(:external :external-cmu
			       :external-4.2
			       :external-4.2-sunos
			       :external-4.2-solaris
			       :external-5.0-solaris
			       :external-5.0-sunos
			       :external-6.1-solaris
			       :external-6.2-solaris
                               :mac :mac-osx :mac-clx
			       :macosx-acl-clx))
      ;; This should work on Ansi compliant lisps.  Try it, if not
      ;; hardcode the pathname.
      (namestring (make-pathname :directory
				  (pathname-directory *load-truename*)))
      #+comment"**FILL THIS IN**"                ;; SET THIS

      ;; Values useful at CMU:
      ;;#-apple "/afs/cs.cmu.edu/project/garnet/"
      ;;#+apple "Macintosh HD:Garnet:"
      ))


;; This function is required for KCL and MCL because they do not properly
;; concatenate directory pathnames
;;
(defun Garnet-Pathnames (subdir dir)
  "This is basically a synonym for merge-pathnames, for Lisps which do
not support it."
  #+(or KCL (and APPLE (not ccl-3)))
  (pathname (concatenate 'string (namestring dir) (namestring subdir)))
  #-(or KCL (and APPLE (not ccl-3)))
  (merge-pathnames subdir dir)
  )

;;; RGA added this function as a cleaner way of handling the differences
;;; between Unix and Mac file naming conventions.  This will loose on pre-ansi
;;; lisps, but I can live with that (I think).
(defun append-directory (pathnme dirstring)
  "This is a little utility for accessing the subdirectory of a
directory."
  (let ((pnd (pathname-directory pathnme))
	(dlist (if (listp dirstring) dirstring
		 (list dirstring))))
    (merge-pathnames (make-pathname :directory (append pnd dlist))
		     pathnme)))



(defun Get-Garnet-Binary-Pathname (version)
  (let ((directory-name
	 (case version
	   (:external-6.1-solaris "bin6.1")
	   (:external-5.0-solaris "bin5.0")
	   (:external-5.0-sunos "bin5.0.sunos")
	   (:external-4.2-sunos "bin.sunos")
	   (:external-4.2-solaris "bin.solaris")
	   (:external-4.1 "bin4.1")
	   (:external-cmu "cmu.bin")	;***
	   (:external "bin")
	   (:sparc-allegro "sparc-allegro-bin")
	   (:sparc-allegro4.1 "sparc-allegro4.1-bin")
	   (:sparc-allegro4.2 "sparc-allegro4.2-bin")
	   (:sparc-cmucl "sparc-cmucl-bin")
	   (:sparc-lucid "sparc-lucid-bin")
	   (:pmax-allegro "pmax-allegro-bin")
	   (:pmax-allegro4.1 "pmax-allegro4.1-bin")
	   (:hp-allegro4.2 "hp-allegro4.2-bin")
	   (:hp-lucid "hp-lucid-bin")
	   (:hp-lucid4.0.3 "hp-lucid403-bin")
	   (:clisp "clisp-bin")
	   (:alpha-lw "alpha-lw-bin")
	   (:mac "bin.mcl")
	   (:mac-osx "bin.osx")
	   (:mac-clx "bin.macclx")
	   (:macosx-acl-clx "bin6.1clx")
	   (t (error "~S is an invalid garnet-version" version)))))
    (unless multiple-garnet-bin-dirs (setq directory-name "bin"))
    (append-directory Your-Garnet-Pathname directory-name)))


(defvar Garnet-Src-Pathname
  (append-directory  Your-Garnet-Pathname "src"))
(defvar Garnet-Binary-Pathname (Get-Garnet-Binary-Pathname garnet-version))
(defvar Garnet-Lib-Pathname
  (append-directory Your-Garnet-Pathname "lib"))
(defvar CLX-Pathname Your-CLX-Pathname)

(defvar Garnet-Utils-Src
  (append-directory Garnet-Src-Pathname "utils"))
(defvar Garnet-Utils-Pathname
  (append-directory Garnet-Binary-Pathname "utils"))
(defvar Garnet-KR-Src
  (append-directory Garnet-Src-Pathname "kr"))
(defvar Garnet-KR-Pathname
  (append-directory Garnet-Binary-Pathname "kr"))
(defvar Garnet-Gworld-Src
  (append-directory Garnet-Src-Pathname "gworld"))
(defvar Garnet-Gworld-Pathname
  (append-directory Garnet-Binary-Pathname "gworld"))
(defvar Garnet-Gem-Src
  (append-directory Garnet-Src-Pathname "gem"))
(defvar Garnet-Gem-Pathname
  (append-directory Garnet-Binary-Pathname "gem"))
(defvar Garnet-Opal-Src
  (append-directory Garnet-Src-Pathname "opal"))
(defvar Garnet-Opal-Pathname
  (append-directory Garnet-Binary-Pathname "opal"))
(defvar Garnet-Inter-Src
  (append-directory Garnet-Src-Pathname "inter"))
(defvar Garnet-Inter-Pathname
  (append-directory Garnet-Binary-Pathname "inter"))
(defvar Garnet-Gesture-Src
  (append-directory Garnet-Src-Pathname "gesture"))
(defvar Garnet-Gesture-Pathname
  (append-directory Garnet-Binary-Pathname "gesture"))
(defvar Garnet-Aggregadgets-Src
  (append-directory Garnet-Src-Pathname "aggregadgets"))
(defvar Garnet-Aggregadgets-Pathname
  (append-directory Garnet-Binary-Pathname "aggregadgets"))
(defvar Garnet-PS-Src
  (append-directory Garnet-Src-Pathname "ps"))
(defvar Garnet-PS-Pathname
  (append-directory Garnet-Binary-Pathname "ps"))
(defvar Garnet-Gadgets-Src
  (append-directory Garnet-Src-Pathname "gadgets"))
(defvar Garnet-Gadgets-Pathname
  (append-directory Garnet-Binary-Pathname "gadgets"))
(defvar Garnet-Debug-Src
  (append-directory Garnet-Src-Pathname "debug"))
(defvar Garnet-Debug-Pathname
  (append-directory Garnet-Binary-Pathname "debug"))
(defvar Garnet-Demos-Src
  (append-directory Garnet-Src-Pathname "demos"))
(defvar Garnet-Demos-Pathname
  (append-directory Garnet-Binary-Pathname "demos"))
(defvar Garnet-Gilt-Src
  (append-directory Garnet-Src-Pathname "gilt"))
(defvar Garnet-Gilt-Pathname
  (append-directory Garnet-Binary-Pathname "gilt"))
(defvar Garnet-C32-Src
  (append-directory Garnet-Src-Pathname "c32"))
(defvar Garnet-C32-Pathname
  (append-directory Garnet-Binary-Pathname "c32"))
(defvar Garnet-Lapidary-Src
  (append-directory Garnet-Src-Pathname "lapidary"))
(defvar Garnet-Lapidary-Pathname
  (append-directory Garnet-Binary-Pathname "lapidary"))
(defvar Garnet-Contrib-Src
  (append-directory Garnet-Src-Pathname "contrib"))
(defvar Garnet-Contrib-Pathname
  (append-directory Garnet-Binary-Pathname "contrib"))
(defvar Garnet-Protected-Eval-Src
  (append-directory Garnet-Src-Pathname "protected-eval"))
(defvar Garnet-Protected-Eval-Pathname
  (append-directory Garnet-Binary-Pathname "protected-eval"))

(defvar Garnet-Bitmap-Pathname
  (append-directory Garnet-Lib-Pathname "bitmaps"))
(defvar Garnet-Pixmap-Pathname
  (append-directory Garnet-Lib-Pathname "pixmaps"))
(defvar Garnet-Gilt-Bitmap-Pathname
  (append-directory Garnet-Lib-Pathname "gilt"))
(defvar Garnet-C32-Bitmap-Pathname
  (append-directory Garnet-Lib-Pathname "c32"))
(defvar Garnet-DataFile-Pathname
  (append-directory Garnet-Lib-Pathname "data"))
(defvar Garnet-Gesture-Data-Pathname
  (append-directory Garnet-Lib-Pathname "gesture"))


;;;----------------------------------------------------------

;;; When compiling, the binaries will be in the same directories as the
;;; source files, so make all the path names be the same
;;;
;;; After compilation is finished, the user should move all the binaries
;;; into their own directories, as specified by the pathnames above.
(defvar *Garnet-Going-To-Compile* nil)

#|
;;; RGA commented this out.  We can just use the garnet-compile
;;; function to compile directly into the source directory, no muss, no fuss.
;;; This way compilation does not depend on a unix utility.
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

|#
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

(defparameter garnet-protected-eval-Loader
  (merge-pathnames "protected-eval-loader" Garnet-Protected-Eval-PathName))


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
    ("protected-eval" . Garnet-Protected-Eval-PathName)
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
    ("protected-eval-src" . Garnet-Protected-eval-Src)
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
              #+carbon-compat"motif-scrolling-labeled-box.cfs"
              #+(and powerpc (not carbon-compat))"motif-scrolling-labeled-box.pfs"
              #-powerpc"motif-scrolling-labeled-box.cfs")
             ((string= name "scrolling-labeled-box-loader")
              "scrolling-labeled-box-loader.li")
             ((string= name "scrolling-unlabeled-box-loader")
              "scrolling-unlabeled-box-loader")
             ((string= name "scrolling-input-string-loader")
              "scrolling-input-string-loader.l")
             (t (error "Truncate-Filename can't map ~S
to a 31 character filename with a .lisp suffix."
                       name)))
            ;; Not a trouble file
            name))))


;; On newer versions of clisp, this makes the renaming .lisp -> .lsp obsolete.
;;#+clisp
;;(when (boundp 'system::*source-file-types*)
;;  (pushnew (pathname ".lisp") system::*source-file-types*)
;;)
#+clisp (setq CUSTOM:*WARN-ON-FLOATING-POINT-CONTAGION* nil)

(defun Garnet-Load (filename)
  (let ((pos (position #\: filename))
	;; RGA This allows MCL to come up to speed with the modern
	;; world.  CVS in the Apple Developers Toolkit is a unix app
	;; and hence treats #\newline as #\linefeed.  MCL is a ported
	;; Mac OS 9 app and hence, by default expects #\newline to be
	;; #\return.  This looses when reading coments.  This
	;; undocumented hack seems to fix that, so we will wrap it
	;; around the compile and load functions.
	#+ccl-5.0(ccl::*linefeed-equals-newline* t)
	)
    (if pos
	(let* ((head (subseq filename 0 pos))
	       (tail (subseq filename (1+ pos)))
	       (prefix (or (eval (cdr (assoc head Garnet-Load-Alist
					     :test #'string=)))
			   (error "Bad prefix ~S~%" head)))
	       ;;; RGA eliminated for MCL-5.0 (presumably running on
	       ;;; Mac OS X)
	       ;;; RGA put back for MCL-5.0, still a problem.
	       (finalname #+apple(garnet-pathnames
				  (truncate-filename tail) prefix)
                          #-apple(garnet-pathnames tail prefix)))
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
  #+cmu         (concatenate 'string "." (c:backend-fasl-file-type c:*backend*))
  #+lispworks             ".afasl"
  #+(and apple (not ccl-5.0)) ".fasl"
  ;; RGA this is a nifty hack to find the right extension type.
  ;; They should promote it.
  #+ccl-5.0               (namestring ccl::*.fasl-pathname*)
  ;; sds: the following should work on any ANSI CL implementation
  #+clisp (concatenate 'string "." (pathname-type (compile-file-pathname "foo.lisp"))))


(defun Garnet-Compile (filename)
  (let ((pos (position #\: filename))
	;; RGA hack to allow reading of unix formatted files.
	#+ccl-5.0(ccl::*linefeed-equals-newline* t)
	)
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
          ;; RGA still need this under MCL 5.0, still uses
          ;; old Mac APIs and truncates filenames.
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
          ;; sds: make sure that bin/foo directory is already there
          (garnet-mkdir-if-needed bin-finalname)
          (compile-file src-finalname :output-file bin-finalname))
        ;; else no colon, abort
        (error "NO COLON, aborting compile"))))
;;; RGA workaround for #\return vs #\linefeed issue in MCL.
(defun do-load (file &rest args)
  "This is a workaround for the #\return vs #\linefeed issue in
   MCL.  Binding this variable seems mean that #\linefeed is treated
   as newlines.  As CVS is a Unix tool, it maps #\newlines to
   #\linefeed. As MCL is a Mac OS 9 program, it expects #\return.
   This should allow us to work with whatever we get."
  (let (
	#+ccl-5.0(ccl::*linefeed-equals-newline* t)
		 )
    (apply #'load file args)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;
;;; The real load
;;;

(format t "...Loading Garnet ...~%")
(setf *load-verbose* t)

;; yuck --- Allegro uses a require form (normally much nicer) to load
;; CLX, instead of giving a filename to load...
(cond
  (load-clx-p
   (defparameter CLX-Loader
     #+lucid (merge-pathnames "windows" CLX-Pathname)
     #+lispworks (merge-pathnames "defsys" CLX-Pathname)
     #+allegro nil			;;uses require form...
     #-(or lucid allegro lispworks) (merge-pathnames "clx" CLX-Pathname))
   (format T "~% %%%%%%% Loading ~A %%%%%%%%~%" #-apple "CLX"
	                                        #+apple "MCL Libraries")
   ;; RGA if we are running apply and CLX, then assume we
   ;; are using X-windows toolkit rather than quickdraw.
   ;; RGA commenting this out for MCL 5.0 as it can't seem to find
   ;; traps, hope that these will all autoload.
   #+(and apple (not clx) (not carbon-compat))
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
   #+(and apple (not clx) carbon-compat)
   (progn ;(require 'traps)
          ;(require 'interfaces)
          (require 'quickdraw)
          ;; For event-handling
          (ccl::require-interface 'types)
          (ccl::require-interface 'quickdraw)
          (ccl::require-interface 'events)
          (ccl::require-interface 'OSutils)
          ;; Next doesn't seem to be around anymore.
          ;;(ccl::require-interface 'OSevents)
          ;; For creating Pen State records (as in gestureinter.lisp)
          (ccl::require-interface 'memory)
          (ccl::require-interface 'QDOffscreen)
          (ccl::require-interface 'controls)
          (ccl::require-interface 'windows)
          (ccl::require-interface 'ColorPicker)
          (terpri))
   #+allegro (require :clx)
   #-(or apple allegro) (load CLX-Loader)
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

(defun get-full-display-name ()
   #+cmu (cdr (assoc :DISPLAY lisp::*environment-list*))
   #+(or allegro lispworks kcl clisp) (sys::getenv "DISPLAY")
   #+(and lucid lcl3.0) (lucid-common-lisp:environment-variable "DISPLAY")
   #+(and lucid (not lcl3.0)) (system:environment-variable "DISPLAY")
   ;; RGA hope this works as a sensible default.  Need a new function to
   ;; support other Lisp.
   ":0"
   )

(defun get-display-name (display)
  (do* ((dlist (coerce display 'list) (cdr dlist))
        (c (car dlist) (car dlist))
        (namelist nil))
       ((or (eq c nil) (eq c '#\:)) (coerce (reverse namelist) 'string))
    (push c namelist)))

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

#+allegro
(require :xcw)

#-(and apple (not clx))
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
	#+allegro
	(excl::ignore-errors
	 (common-windows::open-display-with-auth d-name d-number))
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

;;; RGA --- sometimes I only want to load KR.  Don't need a display we
;;; aren't doing graphics.

(defvar *kr-only* nil
  "Only loading KR so don't need to open displays.")

#-(and apple (not clx))
(unless *kr-only*
  (verify-display-can-be-opened))

;;;
;;; Now back to loading Garnet
;;;

(if load-utils-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :utils))
	(format T "~%****** Utils already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Utils %%%%%%%%~%")
          (do-load Garnet-Utils-Loader)))
    (format T "~%****** NOT Loading Utils *******~%"))

(if load-kr-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :kr))
	(format T "~%****** KR already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading KR %%%%%%%%~%")
          (do-load Garnet-KR-Loader)))
    (format T "~%****** NOT Loading KR *******~%"))

(if load-kr-doc-p (garnet-load "kr:kr-doc"))


#+apple
(if load-gworld-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :gworld))
	(format T "~%****** Gworld already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Gworld %%%%%%%%~%")
          (do-load Garnet-Gworld-loader)))
    (format T "~%****** NOT Loading Gworld *******~%"))

(if load-gem-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :gem))
	(format T "~%****** Gem already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Gem %%%%%%%%~%")
          (do-load Garnet-Gem-Loader)))
    (format T "~%****** NOT Loading Gem *******~%"))

(if load-opal-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :opal))
	(format T "~%****** Opal already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Opal %%%%%%%%~%")
          (do-load Garnet-Opal-Loader)))
    (format T "~%****** NOT Loading Opal *******~%"))

(if load-inter-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :inter))
	(format T "~%****** Interactors already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Interactors %%%%%%%%~%")
          (do-load Garnet-Inter-Loader)))
    (format T "~%****** NOT Loading Interactors *******~%"))

(if load-multifont-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :multifont))
	(format T "~%****** Multifont already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Multifont %%%%%%%%~%")
          (do-load Garnet-Multifont-Loader)))
    (format T "~%****** NOT Loading Multifont *******~%"))

(if load-gesture-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :gesture))
	(format T "~%****** Gestures already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Gestures %%%%%%%%~%")
          (do-load Garnet-Gesture-Loader)))
    (format T "~%****** NOT Loading Gestures *******~%"))

(if load-ps-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :ps))
	(format T "~%****** PS already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading PS %%%%%%%%~%")
          (do-load Garnet-PS-Loader)))
    (format T "~%****** NOT Loading PS *******~%"))

(if load-aggregadgets-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :aggregadgets))
	(format T "~%****** Aggregadgets already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Aggregadgets %%%%%%%%~%")
          (do-load Garnet-Aggregadgets-Loader)))
    (format T "~%****** NOT Loading Aggregadgets *******~%"))

(if load-aggregraphs-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :aggregraphs))
	(format T "~%****** Aggregraphs already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Aggregraphs %%%%%%%%~%")
          (do-load Garnet-Aggregraphs-Loader)))
    (format T "~%****** NOT Loading Aggregraphs *******
** To load aggregraph programs, execute (do-load Garnet-Aggregraphs-Loader)~%"))


(if load-gadgets-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :gadgets))
	(format T "~%****** Gadgets already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Gadgets %%%%%%%%~%")
          (do-load Garnet-Gadgets-Loader)))
    (format T "~%****** NOT Loading Gadgets *******~%"))

(if load-debug-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :debug))
	(format T "~%****** Debugging programs already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Debugging programs %%%%%%%%~%")
          (do-load Garnet-Debug-Loader)))
    (format T "~%****** NOT Loading Debug Files *******
** To load debug programs, execute (do-load Garnet-Debug-Loader)~%"))

(if load-demos-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :demos))
	(format T "~%****** Demos already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Demos %%%%%%%%~%")
          (do-load Garnet-Demos-Loader)))
    (format T "~%****** NOT Loading Demos *******
** To load Demos, execute (do-load Garnet-Demos-Loader)~%"))

(if load-gilt-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :gilt))
	(format T "~%****** Gilt already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Gilt %%%%%%%%~%")
          (do-load Garnet-Gilt-Loader)))
    (format T "~%****** NOT Loading Gilt *******
** To load Gilt, execute (do-load Garnet-Gilt-Loader)~%"))

(if load-c32-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :c32))
	(format T "~%****** C32 already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading C32 %%%%%%%%~%")
          (do-load Garnet-C32-Loader)))
    (format T "~%****** NOT Loading C32 *******
** To load C32, execute (do-load Garnet-C32-Loader)~%"))

(if load-lapidary-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :lapidary))
	(format T "~%****** Lapidary already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Lapidary %%%%%%%%~%")
          (do-load Garnet-Lapidary-Loader)))
    (format T "~%****** NOT Loading Lapidary *******
** To load Lapidary, execute (do-load Garnet-Lapidary-Loader)~%"))

(if load-protected-eval-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :protected-eval))
	(format T "~%****** Protected-eval already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Protected-eval %%%%%%%%~%")
          (do-load Garnet-Protected-Eval-Loader)))
    (format T "~%****** NOT Loading Protected-Eval *******
** To load Protected-Eval, execute (do-load Garnet-Protected-Eval-Loader)~%"))


#|
;; Only set the k-reader if we are loading, not compiling
(if (get :garnet-modules :kr)
  (set-dispatch-macro-character #\# #\k (function kr::k-reader)))
|#

;;; RGA added two auxiliary functions for doing file manipulations.

(defun garnet-shell-exec (command)
  "This is a quick and dirty version of opal:shell-exec used just
   for the compiler.  This currently looses on Mac OS."
  #+apple (declare (ignore command))
  #+allegro
  (excl:run-shell-command command :wait NIL :output :stream
			  :error-output :stream)
  #+lucid
  (lcl:run-program "/bin/sh" :arguments (list "-c" command)
		   :wait NIL :output :stream :error-output :stream)
  #+cmu
  (ext:process-output (ext:run-program "/bin/sh" (list "-c" command)
				       :wait NIL :output :stream))
  #+lispworks
  (foreign::open-pipe command :shell-type "/bin/sh" :buffered t)
  #+clisp
  (ext:make-pipe-input-stream (string command))
  #-(or allegro lucid cmu lispworks clisp)
  (error "Don't know how to execute shell functions in this lisp"))

;;; RGA  This will loose on Windows
(defun garnet-mkdir-if-needed (dirname)
  "Creates the directory if it does not exist."
  #+ansi-cl (ensure-directories-exist dirname :verbose t)
  #-ansi-cl
  ;; sds: ANSI CL spec does not require PROBE-FILE to work on directories
  (unless (probe-file dirname)
    #+apple(create-file dirname)
    #-apple(garnet-shell-exec (format nil "mkdir ~A~%" dirname))))
(defun garnet-copy-files (src-dir bin-dir file-list)
  "Copies a list of files (usually loader files) from source directory
  to binary directory."
  (dolist (file file-list)
    (let ((src (merge-pathnames file src-dir))
	  (dest (merge-pathnames file bin-dir)))
      #+apple(copy-file src dest :if-exists :overwrite)
      #-apple(garnet-shell-exec (format nil "cp ~A ~A~%" src dest)))))

(format t "~%... Garnet Load Complete ...~%")
