!$Id:$
      subroutine c_shape1d(wt,uu,p,ns,knot_vec,shp)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Coded by:    Robert L. Taylor
!      Date:        January 11, 2006
!      Release:     1.0

!      Modified by: Rossana Dimitri
!      Date:        February 08, 2012
!      Release:     1.0

!      Purpose: Compute shape functions for 1-d nurbs
!               and their derivatives up to order 3

!      Reference: Section 4.3, Piegl & Tiller, The NURBS Book

!      Output:
!         Shape functions: 1 = der-1; 2 = der-2; 3 = der-3
!                          4 = shp function.
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'eldata.h'
      include   'iofile.h'
      include  'c_nurb_rec.h'

!     --------------Variable Declarations--------------------------------

      real*8     shp(4,*),knot_vec(*),wt(*)

!     1D nonrational basis functions and derivs in u and v
      real*8     Nshp(4,36)

!     u coordinate at integration point, denominator and derivative sums
      real*8     denom_sum, der_sum_1, der_sum_2, der_sum_3
      real*8     uu

!     NURBS coordinates, counters for loops
      integer    p
      integer    ns
      integer    ic
      integer    ii

!     Temporary variables
      real*8     tem

! ------------------------------------------------------------------
!     Set NURBS coordinate data

      if(nel.gt.60) then
        write(iow,*) ' Number of nodes on element too large'
        call plstop(.true.)
      endif

!     Evaluate 1D shape functions and derivatives each direction
      call dbsfuns(ns,p,ncp2,uu,knot_vec,3,Nshp)  ! U-direction

!     Form basis functions and derivatives dR/du and dR/dv

      denom_sum = 0.0d0
      der_sum_1 = 0.0d0
      der_sum_2 = 0.0d0
      der_sum_3 = 0.0d0

      do ic = 1,p+1
        do ii = 1,4
          shp(ii,ic) = 0.0d0
        end do ! ii
      end do ! ic

      do ic = 1,p+1

!       First Derivative

        shp(1,ic) = Nshp(2,ic) * wt(ic)
        der_sum_1 = der_sum_1 + shp(1,ic)

!       Second Derivative

        if(p.gt.1) then
          shp(2,ic) = Nshp(3,ic) * wt(ic)
          der_sum_2 = der_sum_2 + shp(2,ic)
        endif

!       Third Derivative

        if(p.gt.2) then
          shp(3,ic) = Nshp(4,ic) * wt(ic)
          der_sum_3 = der_sum_3 + shp(3,ic)
        endif

!       Basis functions

        shp(4,ic) = Nshp(1,ic) * wt(ic)
        denom_sum = denom_sum + shp(4,ic)

      enddo ! ic

!     Divide through by denominator
      tem  = 1.0d0/denom_sum

      do ic = 1,p+1
        shp(4,ic) =  shp(4,ic) * tem
        shp(1,ic) = (shp(1,ic) - shp(4,ic)*der_sum_1)*tem
      end do ! ic

!     Second derivatives

      if(p.gt.1) then
        do ic = 1,p+1
          shp(2,ic) = (shp(2,ic) - 2.0d0*shp(1,ic)*der_sum_1
     &                                 - shp(4,ic)*der_sum_2)*tem
        enddo ! ic
      endif ! p > 1

!     Third derivatives

      if(p.gt.2) then
        do ic = 1,p+1
          shp(3,ic) = (shp(3,ic) - 3.0d0*shp(2,ic)*der_sum_1
     &                           - 3.0d0*shp(1,ic)*der_sum_2
     &                                 - shp(4,ic)*der_sum_3)*tem
        enddo ! ic
      endif ! p > 2

      end
