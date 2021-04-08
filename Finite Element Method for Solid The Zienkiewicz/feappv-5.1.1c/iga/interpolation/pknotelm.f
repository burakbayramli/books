!$Id:$
      subroutine pknotelm(knot,length, nelm)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute number of element spaces

!      Inputs:
!        knot(*)   - Knot values
!        length    - Length of knot vector

!      Outputs:
!        nelm      - Number of spaces
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      integer (kind=4) :: length, nelm, ll
      real    (kind=8) :: knot(*)

!     Count number of positive knot intervals
      nelm = 0
      do ll = 2,length
        if(knot(ll).gt.knot(ll-1)) then
          nelm = nelm + 1
        endif
      end do ! ll

      end subroutine pknotelm
