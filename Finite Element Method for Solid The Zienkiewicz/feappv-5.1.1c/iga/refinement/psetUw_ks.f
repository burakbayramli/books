!$Id:$
      subroutine psetUw_ks(lek,UU1, uu, k,s)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      integer    lek, k,s, l
      real*8     UU1(lek), uu

      s = 0
      k = 0
      do l = 1,lek
        if(uu.eq.UU1(l)) then
          s = s + 1
        endif
        if(uu.lt.UU1(l)) then
          k = l - 2
          exit
        endif
      end do ! l

      end subroutine psetUw_ks
