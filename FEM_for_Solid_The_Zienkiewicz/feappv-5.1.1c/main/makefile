include $(FEAPPVHOME5_1)/makefile.in

OBJECTS = feappv.o 

feappv: $(OBJECTS) $(ARFEAPPV)
	ranlib $(ARFEAPPV)
	$(FF) -o feappv $(OBJECTS) $(ARFEAPPV) \
	$(LDOPTIONS)

clean:
	rm -f *.o
	rm -f *genmod.mod
	rm -f *genmod.f90

fclean:
	rm -f feappv
	rm -r -f feappv*.dSYM

%.o: %.f
	$(FF) -c $(FFOPTFLAG) -I$(FINCLUDE) $< -o $@

%.o: %.f90
	$(FF) -c $(FFOPTFLAG) -I$(FINCLUDE) $< -o $@

%.o: %.c
	$(CC) -c $(CCOPTFLAG) -I$(CINCLUDE) $< -o $@

