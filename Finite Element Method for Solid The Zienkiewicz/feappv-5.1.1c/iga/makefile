# N.B.  It is necessary to modify 'makefile.in' before using make.

include $(FEAPPVHOME5_1)/makefile.in

CLEANDIRS = main \
	elements \
	interpolation piegl plot program quadrature refinement

ifeappv: iarchive
	(cd main; make ifeappv)
	@@echo "--> FEAPpv executable made <--"

iarchive:   
	(cd elements; make iarchive)
	(cd interpolation; make iarchive)
	(cd piegl; make iarchive)
	(cd plot; make iarchive)
	(cd program; make iarchive)
	(cd quadrature; make iarchive)
	(cd refinement; make iarchive)
	@@echo "--> FEAPpv Archive updated <--"

install: iarchive ifeappv

clean:
	for i in $(CLEANDIRS); do (cd $$i; make clean); done
	if [ -f $(ARIFEAPPV) ]; then rm $(ARIFEAPPV); fi
	@@echo "--> Feap cleaned <--"

fclean:
	rm -f *.o
	rm -f *.mod
	rm -f *genmod.mod
	rm -f *genmod.f90
	rm -f ifeappv
	rm -r -f ifeap*.dSYM
	(cd main; make fclean)
	@@echo "--> FEAPpv & Files Cleaned <--"

(%.o): %.f 
	@if $(FF) -c -I$(FINCLUDE) $(FFOPTFLAG) $< -o $*.o; then echo $*.o >> $(FL); else false; fi;
	@echo "Building $*.o"

(%.o): %.f90
	@if $(FF) -c -I$(FINCLUDE) $(FFOPTFLAG) $< -o $*.o; then echo $*.o >> $(FL); else false; fi;
	@echo "Building $*.o"

(%.o): %.c
	@if $(CC) -c -I$(FINCLUDE) -I$(CINCLUDE) $(CCOPTFLAG) $< -o $*.o; then echo $*.o >> $(FL); else false; fi;
	@echo "Building $*.o"
