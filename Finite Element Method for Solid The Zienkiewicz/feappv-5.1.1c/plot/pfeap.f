!$Id:$
      subroutine pfeap(xl,yl,siz,color,border)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Put FEAPpv logo on plots

!      Inputs:
!         xl,yl     - Location to place logo
!         siz       - Size of logo
!         color     - Color for plot
!         border    - Border type: <2 = fill; >1 = line

!      Outputs:
!         none      - Plot output to screen/file
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'pdata2.h'

      integer       :: i, ico1, ico2, is,color,border
      real (kind=8) :: xl,yl,dixl,siz,size,sizesm

      integer       :: ifx(11),ify(11),iex(13),iey(13),iln(2)
      integer       :: iax(12),iay(12),ipx(15),ipy(15),ixl(6)
      integer       :: ivx(8) ,ivy(8)

      save

      data ifx / 0,10,45,43,18,16,26,24,14,10, 0/
      data ify / 0,50,50,40,40,30,30,20,20, 0, 0/

      data iex / 0,10,45,43,18,16,26,24,14,12,37,35, 0/
      data iey / 0,50,50,40,40,30,30,20,20,10,10, 0, 0/

      data iax / 0,20,30,40,30,23,18,26,28,14,10, 0/
      data iay / 0,50,50, 0, 0,33,20,20,10,10, 0, 0/

      data ipx / 0,10,30,40,37,29,13,15,24,26,28,26,18,10,0/
      data ipy / 0,50,50,40,24,15,15,25,25,28,38,40,40, 0,0/

      data ivx / 7, 0,10,13,24,34,17, 7/
      data ivy / 0,50,50,17,50,50, 0, 0/

      data ixl /30,70,110,155, 466,510/

!     Save line type

      ico1   = ilno(1)
      ico2   = ilno(2)
      iln(1) = 0
      iln(2) = 1
      call plline(iln)

!     Plot FEAP letters

      size = 200.0/siz
      if(border.le.1) then
        is = 1
      else
        is = 3
      endif

!     F

      call pppcol(color,1)
      dixl = xl*size + ixl(1)
      call dplot((ifx(1)+dixl)/size,ify(1)/size+yl,is)
      do i = 2,11
        call dplot((ifx(i)+dixl)/size,ify(i)/size+yl,2)
      end do
      if(is.eq.1) call clpan

!     E

      dixl = xl*size + ixl(2)
      call dplot((iex(1)+dixl)/size,iey(1)/size+yl,is)
      do i = 2,13
        call dplot((iex(i)+dixl)/size,iey(i)/size+yl,2)
      end do
      if(is.eq.1) call clpan

!     A

      dixl = xl*size + ixl(3)
      call dplot((iax(1)+dixl)/size,iay(1)/size+yl,is)
      do i = 2,12
        call dplot((iax(i)+dixl)/size,iay(i)/size+yl,2)
      end do

      if(is.eq.1) then
        call clpan
      endif

!     P

      call pppcol(color,1)
      dixl = xl*size + ixl(4)
      call dplot((ipx(1)+dixl)/size,ipy(1)/size+yl,is)
      do i = 2,15
        call dplot((ipx(i)+dixl)/size,ipy(i)/size+yl,2)
      end do

      if(is.eq.1) then
        call clpan
      endif

!     P

      call pppcol(color,1)
      sizesm = size*2.5d0
      dixl = xl*sizesm + ixl(5)
      call dplot((ipx(1)+dixl)/sizesm,ipy(1)/sizesm+yl,is)
      do i = 2,15
        call dplot((ipx(i)+dixl)/sizesm,ipy(i)/sizesm+yl,2)
      end do

      if(is.eq.1) then
        call clpan
      endif

!     V

      call pppcol(color,1)
      dixl = xl*sizesm + ixl(6)
      call dplot((ivx(1)+dixl)/sizesm,ivy(1)/sizesm+yl,is)
      do i = 2,8
        call dplot((ivx(i)+dixl)/sizesm,ivy(i)/sizesm+yl,2)
      end do

      if(is.eq.1) then
        call clpan
      endif

!     Restore line type

      ilno(1) = ico1
      ilno(2) = ico2

      end subroutine pfeap
