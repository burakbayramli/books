!$Id:$
      subroutine pstr3d(bb,bpr)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute principal values for 3-d problems.

!      Input:
!         bb(*)  - Stresses in order: bb-xx, bb-yy, bb-zz, bb-xy,
!                  bb-yz, bb-zx
!      Output:
!         bpr(*) - Principal stresses in order: bb-1, bb-2, angle
!                  (degrees): bb-xx to bb-1
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: i
      real (kind=8) :: pi23, tol, thrd, al, b1, b2, b3, c1, c2,c3

      real (kind=8) :: bd(6),bb(6),bpr(3)

      save

      data      pi23/2.0943951023931955d0/ , tol /1.d-12/
      data      thrd/0.3333333333333333d0/

!     Principal value routine for symmetric tensor
!          | bb(1) bb(4) bb(6) |
!     3d = | bb(4) bb(2) bb(5) |
!          | bb(6) bb(5) bb(3) |
!     Compute mean and deviatoric (upper trianglular part) tensors

      b1  = (bb(1) + bb(2) + bb(3))*thrd

      do i = 1,6
        bd(i) = bb(i)
      end do

      do i = 1,3
        bd(i) = bd(i) - b1
      end do

!     Compute 2nd and 3rd invariants of deviator

      c1 = bd(4)*bd(4)
      c2 = bd(5)*bd(5)
      c3 = bd(6)*bd(6)
      b2 = 0.5d0*(bd(1)*bd(1)+bd(2)*bd(2)+bd(3)*bd(3))
     &   + c1 + c2 + c3
      if(b2.le.tol*b1*b1) then
        bpr(1) = b1
        bpr(2) = b1
        bpr(3) = b1
      else
        b3 = bd(1)*bd(2)*bd(3)+(bd(4)+bd(4))*bd(5)*bd(6)
     &     + bd(1)*(c1-c2) + bd(2)*(c1-c3)

!       Set constants

        c1 = 2.d0*sqrt(b2*thrd)
        c2 = 4.d0*b3
        c3 = c1*c1*c1
        al = atan2(sqrt(abs(c3*c3-c2*c2)),c2)*thrd

!       Set principal values

        bpr(1) = b1 + c1*cos(al)
        bpr(2) = b1 + c1*cos(al-pi23)
        bpr(3) = b1 + c1*cos(al+pi23)

      endif

      end subroutine pstr3d
