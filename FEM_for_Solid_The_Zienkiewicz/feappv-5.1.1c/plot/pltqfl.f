!$Id:$
      subroutine pltqfl(ilq,xl,vl,vc,nc,cont)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!      * Changed the dimension of vt and xt according to
!        http://feap.berkeley.edu/forum/index.php?topic=2124
!        Feb 19, 2019
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Fill quadrilaterals with contour values
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      logical       :: cont
      integer       :: i1,i2, i, nc,ilq(*),ilt(3)
      real (kind=8) :: xl(3,*),vl(*),vc(nc)
      real (kind=8) :: xt(3,4),vt(4)

      save

      xt(1,1) = 0.25d0*(xl(1,1) + xl(1,2) + xl(1,3) + xl(1,4))
      xt(2,1) = 0.25d0*(xl(2,1) + xl(2,2) + xl(2,3) + xl(2,4))
      xt(3,1) = 0.25d0*(xl(3,1) + xl(3,2) + xl(3,3) + xl(3,4))
      vt(1)   = 0.25d0*(vl(1) + vl(2) + vl(3) + vl(4))
      call pltcor(1,ilt(1),vt(1),vc,nc)
      do i1 = 1,4
        i2 = mod(i1,4) + 1
        do i = 1,3
          xt(i,2) = xl(i,i1)
          xt(i,3) = xl(i,i2)
        end do ! i
        vt(2)  = vl(i1)
        vt(3)  = vl(i2)
        ilt(2) = ilq(i1)
        ilt(3) = ilq(i2)
        if(cont) then
          call pltecn(xt,vt,vc,nc)
        else
          call pltefl(3,ilt,xt,vt,vc,nc)
        endif

      end do ! i1

      end subroutine pltqfl
