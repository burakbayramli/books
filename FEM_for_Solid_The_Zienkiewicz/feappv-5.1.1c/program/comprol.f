!$Id:$
      subroutine comprol(ie,ix,lagbc, neqj)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Check for Lagrange multiplier & other element equation

!      Inputs:
!         ie(nie,*)    -  Element identifier terms
!         ix(nen1,*)   -  Element connection list
!         lagbc(ndl,*) -  Multiplier equation numbers

!      Outputs:
!         neqj         -  Maximum number of equations
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cdata.h'
      include   'cdat1.h'
      include   'sdata.h'

      integer      :: neqj, n,i, ma
      integer      :: ie(nie,*),ix(nen1,*),lagbc(ndl,*)

      save

!     Adding number of element equations
      do n = 1,numel
        ma   = ix(nen1,n)
        do i = 1,ie(nie-8,ma)
          neqj = max(neqj,lagbc(i,n))
        end do ! i
      end do ! n

      end subroutine comprol
