!$Id:$
      subroutine thfx2d(xl,ul, xx,shp,temp,gradt, ndm,ndf,nel)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Compute thermal gradient

      implicit  none

      integer       :: ndm,ndf,nel, i
      real (kind=8) :: temp
      real (kind=8) :: xl(ndm,*),ul(ndf,*), shp(3,*)
      real (kind=8) :: xx(2),gradt(3)

      save

      gradt(:) = 0.0d0
      xx(:)    = 0.0d0
      do i = 1,nel
        gradt(1:2) = gradt(1:2) + shp(1:2,i)*ul(1,i)
        temp       = temp       + shp(3,i)*ul(1,i)
        xx(:)      = xx(:)      + shp(3,i)*xl(1:2,i)
      end do ! i

      end subroutine thfx2d
