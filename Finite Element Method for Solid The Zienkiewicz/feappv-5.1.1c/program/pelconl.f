!$Id:$
      subroutine pelconl(i,ie,ix,lagbc, ic, ielc,neql)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Lagrange multipler for finite elements

!      Inputs:
!         i            -  Element number
!         ie(nie,*)    -  Element multiplier locator
!         ix(*)        -  List of nodes connected to each element
!         lagbc(ndl,*) -  Multipler equation numbers
!         ic(*)        -  Pointer array

!      Outputs:
!         ielc(*)      -  Holds set of elements connected to each node.
!         neql         -  Largest equation number in list
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cdata.h'
      include   'cdat1.h'
      include   'sdata.h'

      integer       :: i,ma,n,nlm,kk,kp, neql
      integer       :: ie(nie,*), ix(*), lagbc(ndl,*),ic(*),ielc(*)

      ma  = ix(nen1)       ! Material set number
      nlm = ie(nie-8,ma)
      do n = 1,nlm
        kk  = lagbc(n,i)
        neql = max(neql,kk)
        if(kk.gt.0) then
          kp = ic(kk)
          do while (ielc(kp).ne.0 )
            kp = kp - 1
          end do ! while
          ielc(kp) =  i
        endif ! kk > 0
      end do ! n

      end subroutine pelconl
