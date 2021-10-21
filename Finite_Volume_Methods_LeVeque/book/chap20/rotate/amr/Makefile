#
#               Makefile for the amr code:
#
#       To make an executable, type:            make xamr
#       (from the application sub-directory)
#
#       To compile a single file.f type:          make file.o
#       (from the application sub-directory)
#
#       To compile the AMR library files type:    make amrclaw/2dlib
#       (from the AMRCLAWLIBLIB sub-directory)
#
#       To combine all source files type:         make program
#       (from the application sub-directory)
#       (sometimes easier to debug all in one)

#       To combine source + include files type:    make backup
#       (from the application sub-directory
#        date is appended)
#
#

#
#
FFLAGS =  -O
LFLAGS =  -O
LINK	= f77
# .f.o: ; $(LINK) $(FFLAGS) $*.f
OBJECTS = \
  ../qinit.o \
  ../setaux.o \
  ../rpn2ad.o \
  ../rpt2ad.o \
  ../psi.o \
  ../setprob.o 

LIBOBJECTS = \
  $(CLAW)/amrclaw/2d/lib/amr2ez.o \
  $(CLAW)/amrclaw/2d/lib/bc2amr.o \
  $(CLAW)/amrclaw/2d/lib/qad.o \
  $(CLAW)/amrclaw/2d/lib/src2.o \
  $(CLAW)/amrclaw/2d/lib/src1d.o \
  $(CLAW)/amrclaw/2d/lib/advanc.o \
  $(CLAW)/amrclaw/2d/lib/bound.o \
  $(CLAW)/amrclaw/2d/lib/stepgrid.o \
  $(CLAW)/amrclaw/2d/lib/auxcoarsen.o \
  $(CLAW)/amrclaw/2d/lib/fixcapaq.o \
  $(CLAW)/amrclaw/2d/lib/estdt.o \
  $(CLAW)/amrclaw/2d/lib/igetsp.o \
  $(CLAW)/amrclaw/2d/lib/reclam.o \
  $(CLAW)/amrclaw/2d/lib/birect.o \
  $(CLAW)/amrclaw/2d/lib/check.o \
  $(CLAW)/amrclaw/2d/lib/cleanup.o \
  $(CLAW)/amrclaw/2d/lib/colate.o \
  $(CLAW)/amrclaw/2d/lib/errest.o \
  $(CLAW)/amrclaw/2d/lib/flag2refine.o \
  $(CLAW)/amrclaw/2d/lib/allowflag.o \
  $(CLAW)/amrclaw/2d/lib/bufnst.o \
  $(CLAW)/amrclaw/2d/lib/spest.o \
  $(CLAW)/amrclaw/2d/lib/errf1.o \
  $(CLAW)/amrclaw/2d/lib/gfixup.o \
  $(CLAW)/amrclaw/2d/lib/filval.o \
  $(CLAW)/amrclaw/2d/lib/filpatch.o \
  $(CLAW)/amrclaw/2d/lib/filpatch2.o \
  $(CLAW)/amrclaw/2d/lib/filpatch3.o \
  $(CLAW)/amrclaw/2d/lib/prefilp.o \
  $(CLAW)/amrclaw/2d/lib/prefil2.o \
  $(CLAW)/amrclaw/2d/lib/prefil3.o \
  $(CLAW)/amrclaw/2d/lib/flglvl.o \
  $(CLAW)/amrclaw/2d/lib/fluxad.o \
  $(CLAW)/amrclaw/2d/lib/fluxsv.o \
  $(CLAW)/amrclaw/2d/lib/ginit.o \
  $(CLAW)/amrclaw/2d/lib/grdfit.o \
  $(CLAW)/amrclaw/2d/lib/intfil.o \
  $(CLAW)/amrclaw/2d/lib/moment.o \
  $(CLAW)/amrclaw/2d/lib/nestck.o \
  $(CLAW)/amrclaw/2d/lib/prepc.o \
  $(CLAW)/amrclaw/2d/lib/prepf.o \
  $(CLAW)/amrclaw/2d/lib/projec.o \
  $(CLAW)/amrclaw/2d/lib/signs.o \
  $(CLAW)/amrclaw/2d/lib/findcut.o \
  $(CLAW)/amrclaw/2d/lib/smartbis.o \
  $(CLAW)/amrclaw/2d/lib/putnod.o \
  $(CLAW)/amrclaw/2d/lib/putsp.o \
  $(CLAW)/amrclaw/2d/lib/regrid.o \
  $(CLAW)/amrclaw/2d/lib/restrt.o \
  $(CLAW)/amrclaw/2d/lib/setgrd.o \
  $(CLAW)/amrclaw/2d/lib/setuse.o \
  $(CLAW)/amrclaw/2d/lib/stst1.o \
  $(CLAW)/amrclaw/2d/lib/tick.o \
  $(CLAW)/amrclaw/2d/lib/trimbd.o \
  $(CLAW)/amrclaw/2d/lib/update.o \
  $(CLAW)/amrclaw/2d/lib/nodget.o \
  $(CLAW)/amrclaw/2d/lib/upbnd.o  \
  $(CLAW)/amrclaw/2d/lib/basic.o \
  $(CLAW)/amrclaw/2d/lib/drawrg.o \
  $(CLAW)/amrclaw/2d/lib/outval.o \
  $(CLAW)/amrclaw/2d/lib/valout.o \
  $(CLAW)/amrclaw/2d/lib/copysol.o \
  $(CLAW)/amrclaw/2d/lib/outvar.o \
  $(CLAW)/amrclaw/2d/lib/outmsh.o \
  $(CLAW)/amrclaw/2d/lib/outtre.o \
  $(CLAW)/amrclaw/2d/lib/domain.o  \
  $(CLAW)/amrclaw/2d/lib/setflags.o \
  $(CLAW)/amrclaw/2d/lib/shiftset.o \
  $(CLAW)/amrclaw/2d/lib/conck.o \
  $(CLAW)/amrclaw/2d/lib/domshrink.o \
  $(CLAW)/amrclaw/2d/lib/domprep.o \
  $(CLAW)/amrclaw/2d/lib/domup.o \
  $(CLAW)/amrclaw/2d/lib/domcopy.o \
  $(CLAW)/amrclaw/2d/lib/coarsen.o \
  $(CLAW)/amrclaw/2d/lib/intcopy.o \
  $(CLAW)/amrclaw/2d/lib/preintcopy.o \
  $(CLAW)/amrclaw/2d/lib/icall.o \
  $(CLAW)/amrclaw/2d/lib/preicall.o \
  $(CLAW)/amrclaw/2d/lib/step2.o \
  $(CLAW)/amrclaw/2d/lib/b4step2.o \
  $(CLAW)/amrclaw/2d/lib/flux2.o \
  $(CLAW)/amrclaw/2d/lib/limiter.o \
  $(CLAW)/amrclaw/2d/lib/philim.o \
  $(CLAW)/amrclaw/2d/lib/cstore.o \
  $(CLAW)/amrclaw/2d/lib/saveqc.o

SOURCES = \
  ../qinit.f \
  ../setaux.f \
  ../setprob.f \
  ../rpn2ad.f \
  ../rpt2ad.f \
  ../psi.f \
  $(CLAW)/amrclaw/2d/lib/amr2ez.f \
  $(CLAW)/amrclaw/2d/lib/bc2amr.f \
  $(CLAW)/amrclaw/2d/lib/qad.f \
  $(CLAW)/amrclaw/2d/lib/src2.f \
  $(CLAW)/amrclaw/2d/lib/src1d.f \
  $(CLAW)/amrclaw/2d/lib/advanc.f \
  $(CLAW)/amrclaw/2d/lib/bound.f \
  $(CLAW)/amrclaw/2d/lib/stepgrid.f \
  $(CLAW)/amrclaw/2d/lib/auxcoarsen.f \
  $(CLAW)/amrclaw/2d/lib/fixcapaq.f \
  $(CLAW)/amrclaw/2d/lib/estdt.f \
  $(CLAW)/amrclaw/2d/lib/igetsp.f \
  $(CLAW)/amrclaw/2d/lib/reclam.f \
  $(CLAW)/amrclaw/2d/lib/birect.f \
  $(CLAW)/amrclaw/2d/lib/check.f \
  $(CLAW)/amrclaw/2d/lib/cleanup.f \
  $(CLAW)/amrclaw/2d/lib/colate.f \
  $(CLAW)/amrclaw/2d/lib/errest.f \
  $(CLAW)/amrclaw/2d/lib/flag2refine.f \
  $(CLAW)/amrclaw/2d/lib/allowflag.f \
  $(CLAW)/amrclaw/2d/lib/bufnst.f \
  $(CLAW)/amrclaw/2d/lib/spest.f \
  $(CLAW)/amrclaw/2d/lib/errf1.f \
  $(CLAW)/amrclaw/2d/lib/gfixup.f \
  $(CLAW)/amrclaw/2d/lib/filval.f \
  $(CLAW)/amrclaw/2d/lib/filpatch.f \
  $(CLAW)/amrclaw/2d/lib/filpatch2.f \
  $(CLAW)/amrclaw/2d/lib/filpatch3.f \
  $(CLAW)/amrclaw/2d/lib/prefilp.f \
  $(CLAW)/amrclaw/2d/lib/prefil2.f \
  $(CLAW)/amrclaw/2d/lib/prefil3.f \
  $(CLAW)/amrclaw/2d/lib/flglvl.f \
  $(CLAW)/amrclaw/2d/lib/fluxad.f \
  $(CLAW)/amrclaw/2d/lib/fluxsv.f \
  $(CLAW)/amrclaw/2d/lib/ginit.f \
  $(CLAW)/amrclaw/2d/lib/grdfit.f \
  $(CLAW)/amrclaw/2d/lib/intfil.f \
  $(CLAW)/amrclaw/2d/lib/moment.f \
  $(CLAW)/amrclaw/2d/lib/nestck.f \
  $(CLAW)/amrclaw/2d/lib/prepc.f \
  $(CLAW)/amrclaw/2d/lib/prepf.f \
  $(CLAW)/amrclaw/2d/lib/projec.f \
  $(CLAW)/amrclaw/2d/lib/signs.f \
  $(CLAW)/amrclaw/2d/lib/findcut.f \
  $(CLAW)/amrclaw/2d/lib/smartbis.f \
  $(CLAW)/amrclaw/2d/lib/putnod.f \
  $(CLAW)/amrclaw/2d/lib/putsp.f \
  $(CLAW)/amrclaw/2d/lib/regrid.f \
  $(CLAW)/amrclaw/2d/lib/restrt.f \
  $(CLAW)/amrclaw/2d/lib/setgrd.f \
  $(CLAW)/amrclaw/2d/lib/setuse.f \
  $(CLAW)/amrclaw/2d/lib/stst1.f \
  $(CLAW)/amrclaw/2d/lib/tick.f \
  $(CLAW)/amrclaw/2d/lib/trimbd.f \
  $(CLAW)/amrclaw/2d/lib/update.f \
  $(CLAW)/amrclaw/2d/lib/nodget.f \
  $(CLAW)/amrclaw/2d/lib/upbnd.f  \
  $(CLAW)/amrclaw/2d/lib/basic.f \
  $(CLAW)/amrclaw/2d/lib/drawrg.f \
  $(CLAW)/amrclaw/2d/lib/outval.f \
  $(CLAW)/amrclaw/2d/lib/valout.f \
  $(CLAW)/amrclaw/2d/lib/copysol.f \
  $(CLAW)/amrclaw/2d/lib/outvar.f \
  $(CLAW)/amrclaw/2d/lib/outmsh.f \
  $(CLAW)/amrclaw/2d/lib/outtre.f \
  $(CLAW)/amrclaw/2d/lib/domain.f  \
  $(CLAW)/amrclaw/2d/lib/setflags.f \
  $(CLAW)/amrclaw/2d/lib/shiftset.f \
  $(CLAW)/amrclaw/2d/lib/conck.f \
  $(CLAW)/amrclaw/2d/lib/domshrink.f \
  $(CLAW)/amrclaw/2d/lib/domprep.f \
  $(CLAW)/amrclaw/2d/lib/domup.f \
  $(CLAW)/amrclaw/2d/lib/domcopy.f \
  $(CLAW)/amrclaw/2d/lib/coarsen.f \
  $(CLAW)/amrclaw/2d/lib/intcopy.f \
  $(CLAW)/amrclaw/2d/lib/preintcopy.f \
  $(CLAW)/amrclaw/2d/lib/icall.f \
  $(CLAW)/amrclaw/2d/lib/preicall.f \
  $(CLAW)/amrclaw/2d/lib/step2.f \
  $(CLAW)/amrclaw/2d/lib/b4step2.f \
  $(CLAW)/amrclaw/2d/lib/flux2.f \
  $(CLAW)/amrclaw/2d/lib/limiter.f \
  $(CLAW)/amrclaw/2d/lib/philim.f \
  $(CLAW)/amrclaw/2d/lib/cstore.f \
  $(CLAW)/amrclaw/2d/lib/saveqc.f 

IFS = \
  $(CLAW)/amrclaw/2d/lib/calloc.i \
  $(CLAW)/amrclaw/2d/lib/cnodal.i \
  $(CLAW)/amrclaw/2d/lib/cspace.i \
  $(CLAW)/amrclaw/2d/lib/cstats.i \
  $(CLAW)/amrclaw/2d/lib/cdom.i \
  $(CLAW)/amrclaw/2d/lib/cmethod.i\
  $(CLAW)/amrclaw/2d/lib/ciounit.i

xamr: $(OBJECTS)
	$(LINK) $(LFLAGS) $(OBJECTS) $(LIBOBJECTS) -o xamr

backup : $(SOURCES)
	cat $(SOURCES) $(IFS) > amrclaw/2d.`date +%y%m%d` 

program : $(SOURCES)
	cat $(SOURCES) > amrclaw2d.f

clean:
	-rm -f $(OBJECTS) xamr xamrhdf

### DO NOT remove this line - make depends on it ###
