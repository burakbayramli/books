!$Id:$
      logical function visbl(xl)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Find a visible face for quadrilateral facets

!      Inputs:
!         xl(3,*)   - Nodal coordinates for facet

!      Outputs:
!         visbl     - Flag, true if visible from view point
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'ppers.h'

      integer       :: k
      real (kind=8) :: xl(3,4),v1(3),v2(3),v3(3), dot

      save

      do k = 1,3
        v1(k) = xl(k,3) - xl(k,1)
        v2(k) = xl(k,4) - xl(k,2)
      end do

      call vecp(v1,v2,v3)

      do k = 1,3
        v1(k) = e(k) - (xl(k,1)+xl(k,2)+xl(k,3)+xl(k,4))*0.25d0
      end do

      visbl = (dot(v3,v1,3)).gt.0.0d0

      end
