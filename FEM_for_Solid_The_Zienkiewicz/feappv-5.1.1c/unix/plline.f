!$Id:$
      subroutine plline(iln)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose: Set line style and type

!      Inputs:
!         iln(2)    - Line style: 1 = type; 2 = width

!      Outputs:
!         none      - Set output data through commons
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit  none

      include  'pdata2.h'
      include  'pdatps.h'
      include  'plflag.h'
      include  'psdat5.h'
      include  'x11f.h'

      character (len=4) :: cvar

      integer       :: iln(2)

      save

!     Save line type

      ilno(1) = iln(1)
      ilno(2) = iln(2)

!     Change PostScript line attributes

      if (hdcpy) then

!        Change line type (corresponding to old Tek terminal defns)
!             iln(1) = 0   solid
!             iln(1) = 1   dotted
!             iln(1) = 2   dash-dot
!             iln(1) = 3   short dash
!             iln(1) = 4   long dash
!             iln(1) = 5   dot-dot-dash
!             iln(1) = 6   short dash-long dash
!             iln(1) = 7   wide dash

!       Close out a line before changing the line attributes
        if(lstrk .and.
     +    ((iln(1) .ne. dold) .or. (iln(2) .ne. lwold))) then
          call fppsin('s')
          call fppsdu()
          lstrk = .false.
        endif

        if (iln(1) .eq. dold) then
!         Do nothing
          continue
        elseif (iln(1) .eq. 0) then
!         call fppsin(' [] 0 d ')
          call fppsin(' l1 ')
        elseif (iln(1) .eq. 1) then
!         call fppsin(' [5 30] 0 d ')
          call fppsin(' l2 ')
        elseif (iln(1) .eq. 2) then
!         call fppsin(' [40 20 5 20] 0 d ')
          call fppsin(' l3 ')
        elseif (iln(1) .eq. 3) then
!         call fppsin(' [40] 0 d ')
          call fppsin(' l4 ')
        elseif (iln(1) .eq. 4) then
!         call fppsin(' [60] 0 d ')
          call fppsin(' l5 ')
        elseif (iln(1) .eq. 5) then
!         call fppsin(' [5 20 5 40 40 40] 0 d ')
          call fppsin(' l6 ')
        elseif (iln(1) .eq. 6) then
!         call fppsin(' [40 60 80 60] 0 d ')
          call fppsin(' l7 ')
        else
!         call fppsin(' [80] 0 d ')
          call fppsin(' l8 ')
        endif

!       Force a 'moveto' for next line

        lfill = .true.
        if (iln(1) .ne. dold) then
          call fppsdu()
          dold = iln(1)
        endif

!       Change line width

        if(iln(2) .ne. lwold) then
          xx(1) = float(iln(2))
          if( xx(1) .eq. 0.0 ) xx(1) = 1.0
          write(cvar,'(f4.1)') xx(1)*4.0
          call fppsin(' '//cvar//' lw ')
          call fppsdu()
          lwold = nint(xx(1))
        endif
      endif

!     Set line type

      if(iln(1).ge.0) then
         iln(1) = max(0,min(7,iln(1)))
      else
         iln(1) = mod(iln(1)+1, 8)
      endif

!     X11

      xx(1) = min(7,max(0,iln(1)))
      yy(1) = min(5,max(0,iln(2)))
      if(screfl) call gdx11( 13, xx(1), yy(1) )

      end subroutine plline
