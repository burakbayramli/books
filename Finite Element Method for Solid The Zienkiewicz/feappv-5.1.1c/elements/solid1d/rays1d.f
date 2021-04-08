!$Id:$
      subroutine rays1d(d,shp,sig,dd,vl,xl,ndf,ndm,nel)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Stiffness proportional Rayleigh damping residual
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'pmod2d.h'

      integer       :: ndf,ndm,nel, i,j
      real (kind=8) :: d(*),shp(2,*),sig(*),dd(6,6),vl(ndf,*),xl(ndm,*)
      real (kind=8) :: eps(6), xx

      do j = 1,6
        eps(j) = 0.0d0
      end do ! j
      xx = 0.0d0
      do j = 1,nel
        xx     = xx     + shp(1,j)*xl(1,j)
        eps(1) = eps(1) + shp(1,j)*vl(1,j)
        eps(3) = eps(3) + shp(2,j)*vl(1,j)
      end do ! j

!     Set 3-strain (thickness/hoop)

      if(stype.eq.3) then
        eps(3) = eps(3)/xx
      else
        eps(3) = 0.0d0
      endif

      do j = 1,3
        eps(j) = eps(j)*d(78)
      end do ! j

!     Compute stress modification due to damping

      do j = 1,3
        do i = 1,3
          sig(i) = sig(i) + dd(i,j)*eps(j)
        end do ! i
      end do ! j

      end subroutine rays1d
