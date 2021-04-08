!$Id:$
      subroutine plopen

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Open graphics screen to receive plot data

!      Inputs:
!         none

!      Outputs:
!         none
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'pdata2.h'
      include  'pdatap.h'
      include  'pdatps.h'
      include  'plflag.h'
      include  'print.h'

      logical       :: hdcpyo
      integer       :: ifrfl,icol,status,vclrwk

      save

!     If never open start the plot

      if(.not.everon) call plstrt()

!     Open plot device

      if(fopn) return
      fopn   = .true.
      hdcpyo = hdcpy
      icol   = 1

      if(iclear.eq.0) then
        if(screfl) status = vclrwk()
      endif

!     PostScript

      if(iclear.eq.0) then
        iclear = 1

!       Put up border

        if ( hdlogo .and. hdcpy ) then
          hdcpy = .false.
          ifrfl = 1
        endif
        if(bordfl) call plbord(icol)

!       Put up logo for feap

        call pfeap(0.983d0,0.017d0,0.250d0,3,3)
        call pfeap(0.980d0,0.020d0,0.250d0,2,1)

        if (ifrfl .eq. 1) then
          hdcpy = hdcpyo
          ifrfl = 0
        endif

      endif

      end subroutine plopen
