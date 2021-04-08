include $(FEAPPVHOME5_1)/makefile.in

OBJECTS = $(FEAPPVHOME5_1)/main/feappv.o \
	./mpi_start_feap.o \
	./pfe2clean.o \
	./pfe2outf.o \
	./pfe2rve.o \
	./pfe2setd.o \
	./pfe2setio.o \
	./pfe2setrv.o \
	./pfe2solv.o \
	./parsend.o \
	./parstop.o \
	./pform.o \
	./pforma.o \
	./pmacr9.o \
	./rvemat.o \
	./rvesr.o \
	./srvemat.o \
	./tinput.o \
	./trvemat.o \
	./uprob.o

install: $(OBJECTS) $(ARFEAPPV)
	ranlib $(ARFEAPPV)
	$(FFE2) -o fe2feappv $(OBJECTS) $(ARFEAPPV) $(LDOPTIONSFE2)
	dsymutil fe2feappv
	@@echo "--> FE2 FEAPpv executable made <--"

clean:
	rm -f *.o
	rm -f *.mod
	rm -f *genmod.f90

fclean:
	rm -f fe2feappv
	rm -r -f fe2feappv.dSYM

%.o: %.f
	$(FFE2) -c $(FFOPTFLAG) -I$(FINCLUDE) $< -o $@

%.o: %.f90
	$(FFE2) -c $(FFOPTFLAG) -I$(FINCLUDE) $< -o $@

%.o: %.c
	$(CCE2) -c $(CCOPTFLAG) -I$(CINCLUDE) $< -o $@

