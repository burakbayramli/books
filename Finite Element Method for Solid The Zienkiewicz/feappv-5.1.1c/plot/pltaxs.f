!$Id:$
      subroutine pltaxs(xi,ndm,ct)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Draw vectors on screen for direction of coord. axes

!      Inputs:
!         xi(*)     - Location on origin for axes
!         ndm       - Spatial dimension of mesh
!         ct        - Size factor for plot

!      Outputs:
!         none      - Plot outputs to screen/file
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: ndm,m,n
      real (kind=8) :: ct,fac1,fac2,fac3
      real (kind=8) :: dd(3,3),xx(3,5),xi(ndm)

      save

!     Perspective projecton of axes

      call pzero(dd,9)
      do m = 1,ndm
        dd(m,m) = ct
      end do

!     Compute plot location for axes

      do m = 1,ndm
        call pzero(xx,15)
        do n = 1,ndm
          fac1 = dd(n,m)
          xx(n,1) = xi(n)
          xx(n,2) = xx(n,1) + fac1
          xx(n,5) = xx(n,2)
        end do
        fac1 = dd(1,m)*0.1d0
        fac2 = dd(2,m)*0.1d0
        fac3 = dd(3,m)*0.1d0
        xx(1,3) = xx(1,2) - 3.d0*fac1 -  fac2 - fac3
        xx(2,3) = xx(2,2) - 3.d0*fac2 +  fac1 + fac3
        xx(3,3) = xx(3,2) - 3.d0*fac3 +  fac1 + fac2
        xx(1,4) = xx(1,2) - 3.d0*fac1 +  fac2 + fac3
        xx(2,4) = xx(2,2) - 3.d0*fac2 -  fac1 - fac3
        xx(3,4) = xx(3,2) - 3.d0*fac3 -  fac1 - fac2

!       Plot vector

        call plotl(xx(1,1),xx(2,1),xx(3,1),3)
        do n = 2,5
          call plotl(xx(1,n),xx(2,n),xx(3,n),2)
        end do
        call plotl(xx(1,2),xx(2,2),xx(3,2),3)

!       Add label

        call plabl(m)

      end do

      end subroutine pltaxs
