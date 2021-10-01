###############################################################################
#
#  Makefile for 2-D grid library (libgrid.a)
#  =========================================
#
#  (c) J. Blazek, Dec. 21, 1999
#  Modified: February 17, 2014
#
###############################################################################
#
#  EXEC = path to executable
#  O    = extension of object files (Unix, Linux, g77: .o / DOS: .obj)
#  RM   = command to delete files
#  AR   = command to create libraries from object files
#
O = .o
RM = del
AR = ar cru

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
# AR = libtool -o

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

SOURCES = bezier.f ellgrid.f length.f sstretch.f stretch.f tfint.f

OBJECTS = ${SOURCES:.f=$(O)}
.SUFFIXES: .f $(O)

.f$(O):
	$(FC) $(FFLAGS) $*.f

library: $(OBJECTS)
	$(AR) libgrid.a *$(O)

clean:
	$(RM) $(OBJECTS) libgrid.a
