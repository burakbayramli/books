!$Id:$
      subroutine cfunc(shp,xl,ixl,ndm,x)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute coordinates for point defined by local arrays.

!      Inputs:
!         shp(3,*)  - Shape function array
!         xl(ndm,*) - Array of element coordinates
!         ixl(*)    - Element node numbers
!         ndm       - Spatial dimension of mesh

!      Outputs:
!         x(ndm)    - Coordinates of point

!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: k,l,ndm

      integer       :: ixl(*)
      real (kind=8) :: shp(3,*),xl(3,*),x(*)

      do l = 1,ndm
        x(l) = 0.0d0
      end do

      do k = 1,9
        if(ixl(k).gt.0) then
          do l = 1,ndm
            x(l) = x(l) + shp(3,ixl(k))*xl(l,ixl(k))
          end do
        end if
      end do

      end subroutine cfunc
