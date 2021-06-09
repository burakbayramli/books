
# N.B.  It is necessary to modify 'makefile.in' before using make.

include $(FEAPPVHOME5_1)/makefile.in

CLEANDIRS = elements program plot unix user main

feappv: archive
	(cd main; make feappv)
	@@echo "--> FEAPpv executable made <--"

archive:   
	(cd elements; make archive)
	(cd program; make archive)
	(cd plot; make archive)
	(cd unix; make archive)
	(cd user; make archive)
	@@echo "--> FEAPpv Archive updated <--"

install: archive feappv

clean:
	for i in $(CLEANDIRS); do (cd $$i; make clean); done
	if [ -f $(ARFEAPPV) ]; then rm $(ARFEAPPV); fi
	@@echo "--> FEAPpv cleaned <--"

