#
#Makefile for building Garnet
#
CLISP=clisp
CMUCL=lisp
SBCL=sbcl

all: clisp cmucl

clisp:
		$(CLISP) -K full -interactive-debug build.lisp

cmucl:
		$(CMUCL) < build.lisp

sbcl:
		$(SBCL) < build.lisp

clean: clispclean cmuclclean

clispclean:
		rm */*.fas */*.lib
cmuclclean:
		rm */*.x86f
