# 
#Makefile for building Garnet
#
all: clisp cmucl

clisp: 
		clisp -K full build.lisp

cmucl:
		lisp < build.lisp

clean: clispclean cmuclclean
		
clispclean:
		rm */*.fas
cmuclclean:
		rm */*.x86f
