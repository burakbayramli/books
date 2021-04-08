!$Id:$
      subroutine int2dn(l,lint,sg)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Form Nodal quadrature points & weights for 2 dimensions

!      Inputs:
!         l       - Number of points/direction

!      Outputs:
!         lint    - Total number of points
!         sg(3,*) - Array of points and weights
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

!     include  'eldata.h'
      include  'iofile.h'

      integer       :: i,l,lint, x2(9),y2(9),w2(9), x3(16),y3(16),w3(16)
      real (kind=8) :: g,h, sg(3,*)

      save

      data      x2/-1, 1, 1,-1, 0, 1, 0,-1, 0/
      data      y2/-1,-1, 1, 1,-1, 0, 1, 0, 0/
      data      w2/ 1, 1, 1, 1, 4, 4, 4, 4,16/
      data      x3/-3, 3, 3,-3,-1, 1, 3, 3, 1,-1,-3,-3,-1, 1, 1,-1/
      data      y3/-3,-3, 3, 3,-3,-3,-1, 1, 3, 3, 1,-1,-1,-1, 1, 1/
      data      w3/ 1, 1, 1, 1, 3, 3, 3, 3, 3, 3, 3, 3, 9, 9, 9, 9/

!     Set number of total points

      lint = l

!     2x2 integration: 4-node element

      if(l.eq.4) then
        do i = 1,4
          sg(1,i) = dble(x2(i))
          sg(2,i) = dble(y2(i))
          sg(3,i) = 1.d0
        end do ! i

!     3x3 integration: 9-node element

      elseif(l.eq.9) then
        h = 1.d0/9.d0
        do i = 1,9
          sg(1,i) = dble(x2(i))
          sg(2,i) = dble(y2(i))
          sg(3,i) = dble(w2(i))*h
        end do ! i

!     4x4 integration: 16-node element

      elseif(l.eq.16) then
        g = 1.d0/3.d0
        h = 0.0625d0
        do i = 1,16
          sg(1,i) = dble(x3(i))*g
          sg(2,i) = dble(y3(i))*g
          sg(3,i) = dble(w3(i))*h
        end do ! i

!     Error

      else

        write(iow,2000) l
        if(ior.lt.0) then
          write(*,2000) l
        endif
        call plstop(.true.)

      endif

!     Format

2000  format(' *ERROR* INT2DN: Illegal element type =',i16)

      end subroutine int2dn
