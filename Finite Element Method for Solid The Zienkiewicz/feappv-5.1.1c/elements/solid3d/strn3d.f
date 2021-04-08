!$Id:$
      subroutine strn3d(d,ul,th,shp,ndf,nel, eps,ta)

!_____________________________________________________________________c
!     Three dimensional strain calculations

      implicit  none

      include  'cdata.h'

      integer       :: ndf,nel, j
      real (kind=8) :: ta

      real (kind=8) :: d(*),ul(ndf,nen,*),th(*),shp(4,*)
      real (kind=8) :: eps(9,*)

      save

!     Compute stress and strain at point

      do j = 1,9
        eps(j,1) = 0.0d0
        eps(j,2) = 0.0d0
        eps(j,3) = 0.0d0
      end do ! j

!     Compute temperature and coordinates

      ta = -d(9)
      do j = 1,nel
        ta     = ta         + shp(4,j)*th(j)
        eps(1,1) = eps(1,1) + shp(1,j)*ul(1,j,1)
        eps(2,1) = eps(2,1) + shp(2,j)*ul(2,j,1)
        eps(3,1) = eps(3,1) + shp(3,j)*ul(3,j,1)
        eps(4,1) = eps(4,1) + shp(1,j)*ul(2,j,1)
     &                      + shp(2,j)*ul(1,j,1)
        eps(5,1) = eps(5,1) + shp(2,j)*ul(3,j,1)
     &                      + shp(3,j)*ul(2,j,1)
        eps(6,1) = eps(6,1) + shp(3,j)*ul(1,j,1)
     &                      + shp(1,j)*ul(3,j,1)
        eps(1,3) = eps(1,3) + shp(1,j)*ul(1,j,2)
        eps(2,3) = eps(2,3) + shp(2,j)*ul(2,j,2)
        eps(3,3) = eps(3,3) + shp(3,j)*ul(3,j,2)
        eps(4,3) = eps(4,3) + shp(1,j)*ul(2,j,2) + shp(2,j)*ul(1,j,2)
        eps(5,3) = eps(5,3) + shp(2,j)*ul(3,j,2) + shp(3,j)*ul(2,j,2)
        eps(6,3) = eps(6,3) + shp(3,j)*ul(1,j,2) + shp(1,j)*ul(3,j,2)
      end do
      do j = 1,6
        eps(j,2) = eps(j,1) - eps(j,3)
      end do

      end subroutine strn3d
