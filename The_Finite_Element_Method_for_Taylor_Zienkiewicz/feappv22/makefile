
# N.B.  It is necessary to modify 'makefile.in' before using make.

include ./makefile.in

CLEANDIRS = include elements plot program unix user windows main

feappv: archive
	(cd main; make feappv)
	@@echo "--> FEAPpv executable made <--"

archive:   
	(cd program; make archive)
	(cd elements; make archive)
	(cd plot; make archive)
	(cd unix; make archive)
	(cd user; make archive)
	@@echo "--> FEAPpv Archive updated <--"

install: archive feappv

