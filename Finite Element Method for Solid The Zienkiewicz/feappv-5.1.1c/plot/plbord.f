!$Id:$
      subroutine plbord(icl)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Plot border for plots

!      Inputs:
!         icl       - Color for border

!      Outputs:
!         none      - Plot output to screen/file
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'pdata2.h'

      integer      :: icl, icol, iln(2), ilns(2)

      save

!     Draw a border around mesh

      icol = icl
      call pppcol(icol,0)

!     Set line to solid for border

      ilns(1) = ilno(1)
      ilns(2) = ilno(2)
      iln(1)  = 0
      iln(2)  = 3
      call plline(iln)
      call ppbox(0.0003d0, 0.0100d0, 1.279d0, 0.9697d0, 3)

!     Place box lines on figure

      call dplot(0.9700d0, 0.0100d0, 3)
      call dplot(0.9700d0, 0.9796d0, 2)
      call dplot(0.9700d0, 0.8250d0, 3)
      call dplot(1.2793d0, 0.8250d0, 2)
      call dplot(0.9700d0, 0.0960d0, 3)
      call dplot(1.2793d0, 0.0960d0, 2)

!     Place lines for perspective view

      call dplot(1.12465d0, 0.8250d0, 3)
      call dplot(1.12465d0, 0.9796d0, 2)

!     Restore line type

      call plline(ilns)
      ilno(1) = ilns(1)
      ilno(2) = ilns(2)

      end subroutine plbord
