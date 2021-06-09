!$Id:$
      subroutine tplot(x1,y1,tx,mm,nctr)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose: Place a string of text in plot

!      Inputs:
!         x1,y1     - Postion on plot to place text
!         tx(*)     - String of text to plot
!         mm        - Number of characters in text
!         nctr      - Centering information

!      Outputs:
!         none      - Text placed in plot region on screen/file
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit  none

      include  'pdata2.h'
      include  'pdatap.h'
      include  'pdatps.h'
      include  'psdat3.h'
      include  'pdatxt.h'
      include  'plflag.h'
      include  'x11f.h'

      character (len=1) :: tx(*)

      integer       :: mm,nn
      integer       :: i, j, jj, nctr
      real (kind=8) :: x1,y1

      save

!     Set number of characters

      nn = abs(mm)

!     Position for text output

      call dplot(x1,y1,3)

!     X11: Output text

      i      = -(1024+nn)
      x11(1) = xp(2)*xx(2)
      y11(1) = yp(2)*xx(3)*1.28
      y11(2) = nctr
      do j = 1,nn
        jj       = ichar(tx(j))
        x11(j+1) = jj
      end do
      if(screfl) then
        call gdx11(i,x11,y11)
      endif

!     PostScript: Output text

      if (hdcpy) then
        call dplot(x1+dtext,y1,3)
        call fptplt(xp,yp,tx,nn,nctr)
      endif

      end subroutine tplot
