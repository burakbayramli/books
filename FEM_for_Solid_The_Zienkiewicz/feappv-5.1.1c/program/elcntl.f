!$Id:$
      subroutine elcntl(ie,ix,lagbc, ic)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute array for performing assembly of stiffness

!      Inputs:
!          ie(*)        -  Element parameter entries
!          ix(*)        -  Element conectivity array.
!          lagbc(ndl,*) -  Lagrange multiplier entries

!      Outputs:
!          ic(*)        -  Element lagrange multipler equations
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cdata.h'
      include   'cdat1.h'
      include   'sdata.h'

      integer        :: i,inn, n,nlm, ma
      integer        :: ie(nie,*), ix(nen1,*), lagbc(ndl,*), ic(*)

      save

!     Lagrange multiplier elements
      do n = 1,numel
        if(ix(nen1-1,n).ge.0) then
          ma  = ix(nen1,n)       ! material set number
          nlm = ie(nie-8,ma)     ! gets number of element equations
          do i = 1,nlm
            inn     = lagbc(i,n)
            if(inn.gt.0) then
              ic(inn) = ic(inn) + 1
            endif
          end do ! i
        endif
      end do ! n

      end subroutine elcntl
