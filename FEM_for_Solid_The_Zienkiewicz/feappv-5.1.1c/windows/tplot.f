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

      include  'pdatap.h'
      include  'pdatps.h'
      include  'psdat3.h'
      include  'pdatxt.h'
      include  'plflag.h'

      character (len=1) :: tx(*)

      integer       :: mm,nn
      integer       :: i, nctr, vgtxts
      real (kind=8) :: x1,y1

      save

!     Set number of characters

      nn = abs(mm)

!     Position for text output

      if(screfl) i = vgtxts(x1,y1+0.03d0,nn,tx)

!     PostScript: Output text

      if (hdcpy) then
        call dplot(x1+dtext,y1,3)
        call fptplt(xp,yp,tx,nn,nctr)
      endif

      end subroutine tplot
