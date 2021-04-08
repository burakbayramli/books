!$Id:$
      subroutine pc_elmtin(c_el,ix,nen,nen1,nume)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Input C_ELEMENT arrays for mixed nurbs/Bezier elements.

!      Inputs:
!        ix(nen1,nume)  - Element connections
!        nen            - Nodes/elmt
!        nen1           - Dimension of IX
!        nume           - Number of elements

!      Outputs:
!        ix(nen1,nume)  - Element type nurbs
!        c_el(4,4,nume) - Edge extraction operator
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'iofile.h'

      integer    nen,nen1,nume
      integer    c_el(4,nume), ix(nen1,nume)

      logical    errck, pinput, doread
      integer    i,nn
      real*8     td(5)

!     Zero array

      c_el = 0

!     Input the non-zero element values

      doread = .true.

      write(iow,2000) (i,i=1,4)
      do while(doread)
        errck = pinput(td,5)
        nn = nint(td(1))
        if(nn.gt.0) then
          do i = 1,4
            c_el(i,nn) = nint(td(i+1))
          end do ! i
          write(iow,2001) nn,(c_el(i,nn),i=1,4)
        else
          doread = .false.
        endif
      end do ! while

!     Set element type

      do nn = 1,nume
        ix(nen+7,nn) = 6
      end do ! nn

!     Formats

2000  format(/5x,'E l e m e n t   E d g e   E x t r a c t i o n s'//
     &       2x,'El. Edge',4(i3,'-edge'))
2001  format(i10,4i8)

      end
