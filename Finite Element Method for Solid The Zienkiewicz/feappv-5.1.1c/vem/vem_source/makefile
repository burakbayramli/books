include $(FEAPPVHOME5_1)/makefile.in

SOURCES   = $(wildcard $(FSOURCE)*.$(FEXT)) $(wildcard $(CSOURCE)*.$(CEXT)) $(wildcard $(F90SOURCE)*.$(F90EXT)) 
GENMODS   = $(wildcard *genmod.$(F90EXT))
SOURCEST  = $(filter-out $(GENMODS),$(SOURCES))

OBJSF     = $(patsubst %.f,%.o,$(SOURCEST))
OBJSF90   = $(patsubst %.f90,%.o,$(OBJSF))
OBJS      = $(patsubst %.c,%.o,$(OBJSF90))

FL        = built_objs

.SILENT:

varchive:
	@rm -f $(FL); \
	$(MAKE) $(ARVFEAPPV); \
	MKRET=$$?; \
	if [ -f $(FL) ]; then \
	  $(AR) $(ARVFEAPPV) `cat $(FL)`; \
	  rm `cat $(FL)`; \
	  echo "--->`pwd` objects now up-to-date<---"; \
	fi; \
	if [ $$MKRET -ne 0 ]; then false; fi;

$(ARVFEAPPV): $(ARVFEAPPV)($(OBJS))

clean: 
	rm -f *.o
	rm -f *.mod
	rm -f *genmod.f90

(%.o): %.f 
	@if $(FF) -c -I$(FINCLUDE) -I$(NINCLUDE) $(FFOPTFLAG) $< -o $*.o; then echo $*.o >> $(FL); else false; fi;
	@echo "Building $*.o"

(%.o): %.f90
	@if $(FF) -c -I$(FINCLUDE) $(FFOPTFLAG) $< -o $*.o; then echo $*.o >> $(FL); else false; fi;
	@echo "Building $*.o"

(%.o): %.c
	@if $(CC) -c -I$(FINCLUDE) -I$(CINCLUDE) $(CCOPTFLAG) $< -o $*.o; then echo $*.o >> $(FL); else false; fi;
	@echo "Building $*.o"
