#
#               Makefile for the lib code:
#
#       To make an executable, type:            make xclaw
#       (from the application sub-directory)
#
#       To compile a single file.f type:          make file.o
#       (from the application sub-directory)
#
#       To compile the lib library files type:    make claw1
#       (from the claw/clawpack/1d/lib sub-directory)
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
  rp1.o  \
  bc1.o \
  setprob.o \
  src1.o \
  claw1ez.o \
  zeroin.o

LIBOBJECTS = \
  $(CLAWPACK)/1d/lib/setaux.o \
  $(CLAWPACK)/1d/lib/out1.o \
  $(CLAWPACK)/1d/lib/claw1.o \
  $(CLAWPACK)/1d/lib/step1.o \
  $(CLAWPACK)/1d/lib/b4step1.o \
  $(CLAWPACK)/1d/lib/copyq1.o \
  $(CLAWPACK)/1d/lib/limiter.o \
  $(CLAWPACK)/1d/lib/philim.o 

SOURCES = \
  driver.f \
  qinit.f \
  rp1.f \
  src1.f \
  zeroin.f \
  bc1.f \
  setprob.f \
  $(CLAWPACK)/1d/lib/setaux.f \
  claw1ez.f \
  $(CLAWPACK)/1d/lib/out1.f \
  $(CLAWPACK)/1d/lib/claw1.f \
  $(CLAWPACK)/1d/lib/step1.f \
  $(CLAWPACK)/1d/lib/b4step1.f \
  $(CLAWPACK)/1d/lib/copyq1.f \
  $(CLAWPACK)/1d/lib/limiter.f \
  $(CLAWPACK)/1d/lib/philim.f 

xclaw: $(OBJECTS)
	$(LINK) $(LFLAGS) $(OBJECTS) $(LIBOBJECTS) -o xclaw

program : $(SOURCES)
	cat $(SOURCES) > clawpack1.f

### DO NOT remove this line - make depends on it ###
