!$Id:$
      subroutine plstop(eflag)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose: Close any open plot windows and stop execution

!      Inputs:
!         eflag       - If .true. output messagte to screen

!      Outputs:
!         none
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit  none

      include  'iofile.h'
      include  'pdata2.h'
      include  'pdatps.h'
      include  'plflag.h'
      include  'setups.h'
      include  'x11f.h'

      logical       :: eflag

      save

!     Error message

      if(eflag .and. rank.eq.0) then
        write(*,'(a)')
     &       ' --> ERRORS OCCURRED: For details see output file.'
      endif

!     Close PostScript file if open

      if (hdcpy) call fpplcl()

!     X11 device

      if(everon) call gdx11(6,xx,yy)

!     Clear last time history plot data set

      call ptimpl()

!     Close parallel arrays

      call parstop()

!     Close last input & output file

      close(unit=ior, status = 'keep')
      close(unit=iow, status = 'keep')

      stop

      end subroutine plstop
