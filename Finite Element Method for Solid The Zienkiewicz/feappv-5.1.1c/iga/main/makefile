include $(FEAPPVHOME5_1)/makefile.in

OBJECTS = $(FEAPPVHOME5_1)/main/feappv.o # umacr1.o

ifeappv: $(OBJECTS) $(ARFEAPPV) $(ARIFEAPPV)
	ranlib $(ARIFEAPPV)
	$(FF) -Wl,-no_pie -o ifeappv $(OBJECTS) \
	$(ARIFEAPPV) \
	$(ARFEAPPV) $(LDOPTIONS)
	dsymutil ifeappv

# UBUNTU and other GCC loader type machines
# Replace $(ARIFEAPPV) \ by:
#    -L$(FEAPPVHOME5_1)/iga \
#    -Wl,-whole-archive -lfeappviga -Wl,-no-whole-archive \

clean:
	rm -f *.o
	rm -f *genmod.mod
	rm -f *genmod.f90

fclean:
	rm -f ifeappv
	rm -r -f ifeappv.dSYM
	@@echo "--> iFeapPV cleaned <--"

%.o: %.f
	$(FF) -c $(FFOPTFLAG) -I$(FINCLUDE) -I$(NINCLUDE) $< -o $@

%.o: %.f90
	$(FF) -c $(FFOPTFLAG) -I$(FINCLUDE) $< -o $@

%.o: %.c
	$(CC) -c $(CCOPTFLAG) -I$(CINCLUDE) $< -o $@
