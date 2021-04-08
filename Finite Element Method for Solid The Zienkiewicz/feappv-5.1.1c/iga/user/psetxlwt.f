!$Id:$
      subroutine psetxlwt(ns,x,w, xl,wt, len,ndm)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set local coordinate and weight arrays from connection
!               list

!      Inputs:

!      Outputs:
!-----[--.----+----.----+----.-----------------------------------------]

      implicit   none

      integer    len,ndm, l,n
      integer    ns(*)
      real*8     x(ndm,*),w(*), xl(ndm,*),wt(*)

      do l = 1,len
        do n = 1,ndm
          xl(n,l) = x(n,ns(l))
        end do ! n
        wt(l) = w(ns(l))
      end do ! l

      call mprint(xl,ndm,len,ndm,'XL_nsurf')
      call mprint(wt,  1,len,  1,'WT_nsurf')

      end
