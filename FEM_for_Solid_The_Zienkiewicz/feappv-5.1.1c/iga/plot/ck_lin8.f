!$Id:$
      subroutine ck_lin8 ( ix, xl )

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Check 8-node brick for negative jacobian

!      Inputs:
!         ix(*)     - List of nodes for element
!         xl(3,*)   - Coordinate array

!      Outputs:
!         Renumbered element

!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer   l, ineg, ix(*)
      real*8    detj, rst(3,8), xl(3,*), shp(4,8)

      save

      data      rst/-1.d0,-1.d0,-1.d0,   1.d0,-1.d0,-1.d0,
     &               1.d0, 1.d0,-1.d0,  -1.d0, 1.d0,-1.d0,
     &              -1.d0,-1.d0, 1.d0,   1.d0,-1.d0, 1.d0,
     &               1.d0, 1.d0, 1.d0,  -1.d0, 1.d0, 1.d0/

!     Compute jacobian at each corner of element

      ineg = 0
      do l = 1,8
        call bjac3d ( rst(1,l) , xl, 3, shp, detj )
        if(detj.le.0.0d0) then
          ineg = ineg + 1
        endif
      end do ! l
      if(ineg.eq.8) then
        do l = 1,4
          ineg    = ix(l)
          ix(l  ) = ix(l+4)
          ix(l+4) = ineg
        end do ! l
      endif

      end
