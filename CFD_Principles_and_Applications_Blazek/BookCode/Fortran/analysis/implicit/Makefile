###############################################################################
#
#  Makefile for IMPLIC
#  ===================
#
#  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
#  Created : December 21, 1999
#  Modified: February 4, 2014
#
###############################################################################
#
#  EXEC = path to the executable
#  O    = extension of object files (Unix, Linux, g77: .o / DOS: .obj)
#  RM   = command to delete files
#
EXEC = implic.exe
O = .o
RM = del

#### PC - GNU Fortran
#
LD = gfortran
FC = gfortran
FFLAGS = -O3 -Wall -c
LDFLAGS = -O3 -s -o

#### Mac OS X - f2c
#
# LD = f77
# FC = f77
# FFLAGS = -O -u -trapuv -c
# LDFLAGS = -O -trapuv -o

#### IBM - RS6000
#
# LD = xlf
# FC = xlf
# FFLAGS = -O3 -c
# LDFLAGS = -o

#### SGI - Octane
#
# LD = f77
# FC = f77
# FFLAGS = -64 -mips4 -O2 -c
# LDFLAGS = -64 -mips4 -O2 -o

#### SUN Ultra
#
# LD = f77
# FC = f77
# FFLAGS = -fast -xarch=v8plusa -c
# LDFLAGS = -fast -xarch=v8plusa -o

SOURCES = implic.f

OBJECTS = ${SOURCES:.f=$(O)}
.SUFFIXES: .f $(O)

.f$(O):
	$(FC) $(FFLAGS) $*.f

$(EXEC): $(OBJECTS)
	$(FC) $(LDFLAGS) $(EXEC) $(OBJECTS)

clean:
	$(RM) $(OBJECTS) $(EXEC)
