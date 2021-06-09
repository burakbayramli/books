!$Id:$
      subroutine tint2dn(l,lint,el)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set quadrature points & weights for triangular element

!      Inputs:
!         l       - Number of nodes on element

!      Outputs:
!         lint    - Total number of points
!         el(4,*) - Area coordinate points and weights for quadrature
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'

      integer       :: i,j,l, lint
      real (kind=8) :: el(4,*), gg

      save

!     3-point nodal integration

      if(l.eq.3) then
        do j = 1,3
          do i = 1,3
            el(i,j) = 0.0d0
          end do ! i
          el(j,j) = 1.d0
          el(4,j) = 1.d0/3.d0
        end do ! j

        lint    = 3

!     6-point nodal integration

      elseif(l.eq.6) then
        gg = 1.d0/15.d0
        do j = 1,3
          do i = 1,3
            el(i,j) = 0.0d0
          end do ! i
          el(j,j) = 1.d0
          el(4,j) = gg
        end do ! j
        el(1,4) = 0.5d0
        el(2,4) = 0.5d0
        el(3,4) = 0.0d0
        el(4,4) = 4.d0*gg

        el(1,5) = 0.0d0
        el(2,5) = 0.5d0
        el(3,5) = 0.5d0
        el(4,5) = el(4,4)

        el(1,6) = 0.5d0
        el(2,6) = 0.0d0
        el(3,6) = 0.5d0
        el(4,6) = el(4,4)

        lint    = 6

!     7-point nodal integration

      elseif(l.eq.7) then
        gg = 1.d0/15.d0
        do j = 1,3
          do i = 1,3
            el(i,j) = 0.0d0
          end do ! i
          el(j,j) = 1.00d0
          el(4,j) = 0.05d0
        end do ! j
        el(1,4) = 0.5d0
        el(2,4) = 0.5d0
        el(3,4) = 0.0d0
        el(4,4) = 2.0d0/15.0d0

        el(1,5) = 0.0d0
        el(2,5) = 0.5d0
        el(3,5) = 0.5d0
        el(4,5) = el(4,4)

        el(1,6) = 0.5d0
        el(2,6) = 0.0d0
        el(3,6) = 0.5d0
        el(4,6) = el(4,4)

        el(1,7) = 1.d0/3.d0
        el(2,7) = el(1,7)
        el(3,7) = el(1,7)
        el(4,7) = 0.45d0

        lint    = 7

      else
        if(ior.lt.0) then
          write(*,2000) l
        endif
        write(iow,2000) l
        call plstop(.true.)
      endif

!     Format

2000  format(' *ERROR* TINT2DN: Wrong quadrature, nel =',i3)

      end subroutine tint2dn
