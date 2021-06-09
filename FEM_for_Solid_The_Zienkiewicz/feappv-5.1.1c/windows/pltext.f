!$Id:$
      subroutine pltext(x,y,il,str)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose: Place graphics text at screen positions

!      Inputs:
!         x,y       - Screen coordinates for graphics text
!         il        - Length of text
!         str(*)    - Text string to place

!      Outputs:
!         none      - Plot outputs to screen/file
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit  none

      include  'pdatxt.h'

      character (len=1) :: str(*)
      integer       :: il
      real (kind=8) :: x,y

      save

!     Set coordinates for plot

      dtext = 0.00d0

      call tplot(x,y,str,il,0)

      end subroutine pltext
