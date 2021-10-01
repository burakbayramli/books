###############################################################################
#
#  Makefile for EUL1DMG
#  ====================
#
#  (c) J. Blazek, Dec. 22, 2004
#  Modified: February 17, 2014
#
###############################################################################
#
#  EXEC = path to executable
#  O    = extension of object files (Unix: .o / DOS: .obj)
#  RM   = command to delete files
#
EXEC = ..\runmg\eul1dmg.exe
O = .o
RM = del

#### PC - GNU Fortran 77
#
LD = gfortran
FC = gfortran
FFLAGS = -O3 -Wall -c
LDFLAGS = -O3 -s -o

#### Mac OS X - f2c
#
# LD = f77
# FC = f77
# FFLAGS = -O -C -u -trapuv -c
# LDFLAGS = -O -C -trapuv -o

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

SOURCES = bcond.f conver.f dissip.f dissip2.f fluxcen.f fluxcusp.f fluxroe.f \
          getaddr.f getparms.f iniflow.f inigrid.f inject.f interpol.f \
          irsmoo.f lrcusp.f lrroe.f main.f mgsolver.f output.f prolong.f \
          rchar.f restrict.f rgrid.f rsolut.f solver.f srcterm.f tstep.f \
          wsolut.f

OBJECTS = ${SOURCES:.f=$(O)}
.SUFFIXES: .f $(O)

.f$(O):
	$(FC) $(FFLAGS) $*.f

$(EXEC): $(OBJECTS)
	$(FC) $(LDFLAGS) $(EXEC) $(OBJECTS)

clean:
	$(RM) $(OBJECTS) $(EXEC)
