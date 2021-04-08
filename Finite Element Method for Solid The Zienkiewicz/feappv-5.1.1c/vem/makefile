# N.B.  It is necessary to modify 'makefile.in' before using make.

include $(FEAPPVHOME5_1)/makefile.in

CLEANDIRS = vem_source

vfeap: varchive
	(cd main; make vfeap)
	@@echo "--> VEM Feappv executable made <--"

varchive:   
	(cd vem_source; make varchive)
	@@echo "--> Feappv VEM Archive updated <--"

install: varchive vfeappv

clean:
	for i in $(CLEANDIRS); do (cd $$i; make clean); done
	if [ -f $(ARVFEAPPV) ]; then rm $(ARVFEAPPV); fi
	@@echo "--> Feappv VEM cleaned <--"

fclean:
	rm -f *.o
	rm -f *.mod
	rm -f *genmod.mod
	rm -f *genmod.f90
	rm -f vfeappv
	rm -r -f vfeap*.dSYM
	(cd main; make fclean)
	@@echo "--> Feappv & Files Cleaned <--"

(%.o): %.f 
	@if $(FF) -c -I$(FINCLUDE) $(FFOPTFLAG) $< -o $*.o; then echo $*.o >> $(FL); else false; fi;
	@echo "Building $*.o"

(%.o): %.f90
	@if $(FF) -c -I$(FINCLUDE) $(FFOPTFLAG) $< -o $*.o; then echo $*.o >> $(FL); else false; fi;
	@echo "Building $*.o"

(%.o): %.c
	@if $(CC) -c -I$(FINCLUDE) -I$(CINCLUDE) $(CCOPTFLAG) $< -o $*.o; then echo $*.o >> $(FL); else false; fi;
	@echo "Building $*.o"
