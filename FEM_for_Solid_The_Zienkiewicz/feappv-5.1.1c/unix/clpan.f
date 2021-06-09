!$Id:$
      subroutine clpan()

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose: Close panel plots

!      Inputs:
!         none

!      Outputs:
!         none
!-----[--+---------+---------+---------+---------+---------+---------+-]

      implicit  none

      include  'pdatap.h'
      include  'pdatps.h'
      include  'pdataq.h'
      include  'plflag.h'
      include  'psdat3.h'
      include  'x11f.h'

      integer       :: ii

      save

!     Close panel for filled plots

!     Fill panel using X11

      if(ipan.ge.3) then
        do ii=1, ipan
          x11(ii) = xp(ii)*xx(2)
          y11(ii) = yp(ii)*xx(3)*1.28
        end do

        if(screfl) call gdx11(ipan+1024,x11,y11)

!       Fill panel for PostScript

        if (hdcpy) call fppspl(ipan,xp,yp)

      endif

!     Reinitialize panel counter

      ipan = 0

      end subroutine clpan
