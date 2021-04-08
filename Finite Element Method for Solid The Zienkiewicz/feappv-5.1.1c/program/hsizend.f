!$Id:$
      subroutine hsizend(xl, ndm,nel)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute maximum and minimum element size

!      Inputs:
!         xl(ndm,nel) - Element nodal coordinates
!         ndm         - Spatial dimension of mesh
!         nel         - Number of nodes on element

!      Outputs:
!         hsize(2)    - Element min/max size
!                       1 = minimum; 2 = maximum
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'qudshp.h'

      integer       :: ndm,nel, m,n,i
      real (kind=8) :: xl(ndm,nel), dh, dhmax

      save

      dhmax = 0.0d0
      do n = 1,nel-1
        do m = n+1,nel
          dh = (xl(1,n) - xl(1,m))**2
          do i = 2,ndm
            dh = dh + (xl(i,n) - xl(i,m))**2
          end do ! i
          dhmax = max(dhmax,dh)
        end do ! m
      end do ! n
      dhmax    = sqrt(dhmax)
      hsize(1) = dhmax
      dhmax    = dhmax*1.d-10
      do n = 1,nel-1
        do m = n+1,nel
          dh = (xl(1,n) - xl(1,m))**2
          do i = 2,ndm
            dh = dh + (xl(i,n) - xl(i,m))**2
          end do ! i
          dh = sqrt(dh)
          if(dh.gt.dhmax) then
            hsize(1) = min(hsize(1),dh)
          endif
          hsize(2) = max(hsize(2),dh)
        end do ! m
      end do ! n

      end subroutine hsizend
