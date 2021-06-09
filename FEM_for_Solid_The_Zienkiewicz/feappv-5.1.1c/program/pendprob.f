!$Id:$
      subroutine pendprob

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: End a problem

!      Inputs:
!        None

!      Outputs:
!        Close tplot files and clean up memory
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'allotd.h'
      include   'allotn.h'
      include   'cdata.h'
      include   'comfil.h'
      include   'comsav.h'
      include   'contrl.h'
      include   'counts.h'
      include   'debugs.h'
      include   'endata.h'
      include   'iofile.h'
      include   'pdatps.h'
      include   'rdata.h'
      include   'rdat1.h'
      include   'sdata.h'
      include   'tdata.h'

      logical        :: errs

      save

!     Set last data in tplots for multiple problem case

      if(prob_on) then

        inquire(unit=ior,name=fnamp,exist=errs)

        if(errs) then

!         Clear plot files, delete scratch files and close output file

          if(hdcpy) call fpplcl()
          rfl = .false.
          call ptimpl()

!         Restore master output file number and name

          close(iow, status = 'keep')
          iow     = iow_sav
          fout    = fout_sav

        endif
      endif

      end subroutine pendprob
