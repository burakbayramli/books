!$Id:$
      subroutine pt_shp(u,i,shp,wt,knot,ord)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  1-d  shape functions and natural derivatives

!      Inputs:
!         u         - Knot coordinate for evaluation
!         i         - Span number
!         wt(*)     - Weight on control point
!         knot(*)   - Knot vector
!         ord       - Order of knot vector

!      Outputs:
!         shp(3,*)  - Shape functions and derivatives
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'iofile.h'

      integer (kind=4) :: ord
      real    (kind=8) :: shp(3,*),wt(*),knot(*),Nshp(3,ord+1)

      integer (kind=4) :: np, i,ii,ic,is
      real    (kind=8) :: u,denom_sum,der_sum_1,der_sum_2,tem

!     Evaluate 1D shape functions and derivatives

      np    = ord + 1
      call dersbasisfuns(i-1, u, ord, 2, knot, Nshp)  ! U-direction

!     Form basis functions and derivatives dR/du and dR/dv

      denom_sum = 0.0d0
      der_sum_1 = 0.0d0
      der_sum_2 = 0.0d0

      do ic = 1,np
        do ii = 1,3
          shp(ii,ic) = 0.0d0
        end do ! ii
      end do ! ic

      is = i - np
      do ic = 1,np

!       First Derivative

        shp(1,ic) = Nshp(2,ic) * wt(ic+is)
        der_sum_1  = der_sum_1  + shp(1,ic)

!       Second Derivative

        if(ord.gt.1) then
          shp(2,ic) = Nshp(3,ic) * wt(ic+is)
          der_sum_2 = der_sum_2  + shp(2,ic)
        endif

!       Basis functions

        shp(3,ic) = Nshp(1,ic) * wt(ic+is)
        denom_sum = denom_sum  + shp(3,ic)

      enddo ! ic

!     Divide through by denominator

      tem  = 1.0d0/denom_sum
      do ic = 1,np
        shp(3,ic) =  shp(3,ic) * tem
        shp(1,ic) = (shp(1,ic) - shp(3,ic)*der_sum_1)*tem
      end do ! ic

!     Second derivatives

      if(ord.gt.1) then
        do ic = 1,np
          shp(2,ic) = (shp(2,ic) - 2.0d0*shp(1,ic)*der_sum_1
     &                                 - shp(3,ic)*der_sum_2)*tem
        enddo ! ic
      endif

      end subroutine pt_shp
