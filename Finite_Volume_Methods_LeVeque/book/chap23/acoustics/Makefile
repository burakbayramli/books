#
#               Makefile for the clawpack code:
#
#       To make an executable, type:            make xclaw
#       (from the application sub-directory)
#
#       To compile a single file.f type:          make file.o
#       (from the application sub-directory)
#
#       To compile the lib library files type:    make lib
#       (from the claw/clawpack/2d/lib sub-directory)
#
#       To combine all source files type:         make program
#       (from the application sub-directory)
#       (sometimes easier to debug all in one)
#
#
FFLAGS =  -O
LFLAGS =  -O
LINK	= f77
# .f.o: ; $(LINK) $(FFLAGS) $*.f
OBJECTS = \
  driver.o \
  qinit.o \
  rpn2acvq.o \
  rpt2acvq.o \
  setaux.o \
  mapc2p.o \
  bc2quad.o \
  setprob.o 

LIBOBJECTS = \
  $(CLAW)/clawpack/2d/lib/claw2ez.o \
  $(CLAW)/clawpack/2d/lib/b4step2.o \
  $(CLAW)/clawpack/2d/lib/out2.o \
  $(CLAW)/clawpack/2d/lib/restart2.o \
  $(CLAW)/clawpack/2d/lib/claw2.o \
  $(CLAW)/clawpack/2d/lib/step2.o \
  $(CLAW)/clawpack/2d/lib/step2ds.o \
  $(CLAW)/clawpack/2d/lib/dimsp2.o \
  $(CLAW)/clawpack/2d/lib/flux2.o \
  $(CLAW)/clawpack/2d/lib/copyq2.o \
  $(CLAW)/clawpack/2d/lib/limiter.o \
  $(CLAW)/clawpack/2d/lib/philim.o \
  $(CLAW)/clawpack/2d/lib/src2.o 

SOURCES = \
  driver.f \
  qinit.f \
  rpn2acvq.f \
  rpt2acvq.f \
  setaux.f \
  mapc2p.f \
  bc2quad.f \
  setprob.f \
  $(CLAW)/clawpack/2d/lib/claw2ez.f \
  $(CLAW)/clawpack/2d/lib/b4step2.f \
  $(CLAW)/clawpack/2d/lib/out2.f \
  $(CLAW)/clawpack/2d/lib/restart2.f \
  $(CLAW)/clawpack/2d/lib/claw2.f \
  $(CLAW)/clawpack/2d/lib/step2.f \
  $(CLAW)/clawpack/2d/lib/step2ds.f \
  $(CLAW)/clawpack/2d/lib/dimsp2.f \
  $(CLAW)/clawpack/2d/lib/flux2.f \
  $(CLAW)/clawpack/2d/lib/copyq2.f \
  $(CLAW)/clawpack/2d/lib/limiter.f \
  $(CLAW)/clawpack/2d/lib/philim.f \
  $(CLAW)/clawpack/2d/lib/src2.f 

xclaw: $(OBJECTS)
	$(LINK) $(LFLAGS) $(OBJECTS) $(LIBOBJECTS) -o xclaw

program : $(SOURCES)
	cat $(SOURCES) > claw2program.f

### DO NOT remove this line - make depends on it ###
