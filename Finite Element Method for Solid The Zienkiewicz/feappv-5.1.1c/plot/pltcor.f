!$Id:$
      subroutine pltcor(nel,ic,v,vc,nc)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute number of contour at element corners for
!               use by contour plot routines

!      Inputs:
!         nel       - Number of nodes on element
!         v(*)      - Contour value at node
!         vc(*)     - Contour values to plot
!         nc        - Number of contours plotted

!      Outputs:
!         ic(*)     - Contour number at node
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: nel,nc, i,n
      integer       :: ic(nel)
      real (kind=8) :: v(nel),vc(nc)

      save

      do i = 1,nel
        ic(i) = 1
      end do ! i
      do n = 1,nc
        do i = 1,nel
          if(v(i).ge.vc(n)) then
            ic(i) = n + 1
          endif
        end do ! i
      end do ! n

      end subroutine pltcor
