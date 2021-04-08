!$Id:$
      subroutine psetPw(les, ndm, csides, x,w, Pw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cnurb.h'
      integer    les,ndm, l,j, csides(*)
      real*8     x(ndm,*), w(*), Pw(ndm+1,*)

      do l = 1,les
        do j = 1,ndm
          Pw(j,l) = x(j,csides(l))*w(csides(l))
        end do ! j
        Pw(ndm+1,l) = w(csides(l))
      end do ! l

      end subroutine psetPw
