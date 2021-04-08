!$Id:$
      subroutine psetQw(leq, ndm, nd1, Qw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      integer    leq,ndm,nd1, n,j
      real*8     Qw(nd1,leq)

      do n = 1,leq
        if(Qw(nd1,n).gt.0.0d0) then
          do j = 1,ndm
            Qw(j,n) = Qw(j,n)/Qw(nd1,n)
          end do ! j
        endif
      end do ! n

      end subroutine psetQw
