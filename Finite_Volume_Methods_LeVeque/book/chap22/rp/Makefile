#
#  Makefile for Riemann solvers:
#
#  Type "make" or "make rp" to create .o files needed in 
#  applications directories
#
#
FFLAGS =  -c
LFLAGS =  
LINK	= f77
.f.o: ; $(LINK) $(FFLAGS) $*.f

RPOBJECTS = \
  rpn2elv.o \
  rpt2elv.o

RPSOURCES = \
  rpn2elv.f \
  rpt2elv.f

rp: $(RPOBJECTS)


### DO NOT remove this line - make depends on it ###
