!$Id:$
      subroutine plopen()

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--+---------+---------+---------+---------+---------+---------+-]
!     Purpose: Open graphics screen to receive plot data

!      Inputs:
!         none

!      Outputs:
!         none
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit  none

      include  'debugs.h'
      include  'pdata2.h'
      include  'pdatap.h'
      include  'pdatps.h'
      include  'plflag.h'
      include  'print.h'
      include  'x11f.h'

      logical       :: hdcpyo
      integer       :: i,ifrfl,icol

      save

!     Open plot device

      if(fopn) return
      fopn   = .true.
      hdcpyo = hdcpy
      icol   = 1 ! White

!     If never open start the plot

      if(.not.everon) call plstrt()

!     X11 device

      if (screfl) then

        call gdx11(1,xp,yp)
        call gdx11(7,xx,yy)
        if ( iclear .eq. 0 )then
           call gdx11(2,xp,yp)
        endif

        if(xx(3)*1.28 .gt. xx(2)) then
          xx(3) = xx(2)/1.28
        else
          xx(2) = xx(3)*1.28
        endif

        if(debug) then
          write(*,2000) (xx(i),i = 2,6)
        endif

        ipan = 0

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

        icol = 2 ! Red
        call pfeap(0.98d0,0.02d0,0.250d0,icol,1)

        if (ifrfl .eq. 1) then
          hdcpy = hdcpyo
          ifrfl = 0
        endif

      endif

!     Format ("debug" mode only)

2000  format('  X Length in cm.     ',f10.5/
     &       '  Y Length in cm.     ',f10.5/
     &       '  X Pixels in cm.     ',f10.5/
     &       '  Y Pixels in cm.     ',f10.5/
     &       '  No. Forground colors',f10.5/)

      end subroutine plopen
