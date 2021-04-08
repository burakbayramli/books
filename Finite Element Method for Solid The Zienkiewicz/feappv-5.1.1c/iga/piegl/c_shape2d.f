!$Id:$
      subroutine c_shape2d(wt,uu,vv,ord,kno,lk,nel,ncp,shp)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Coded by:    Robert L. Taylor
!      Date:        January 11, 2006
!      Release:     1.0

!      Modified by: Rossana Dimitri
!      Date:        April 25, 2012
!      Release:     1.0

!      Purpose: Compute shape functions for 2-d nurbs
!               and their derivatives up to order 3
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include  'c_nurb06_rec.h'

!     --------------Variable Declarations--------------------------------

      real*8     shp(10,nel),kno(lk,2),wt(nel)

!     1D nonrational basis functions and derivs in u and v
      real*8,    allocatable :: Nshp(:,:),Mshp(:,:)

!     u coordinate at integration point, denominator and derivative sums
      real*8     denom_sum
      real*8     dsum_U1,dsum_U2,dsum_U3
      real*8     dsum_V1,dsum_V2,dsum_V3
      real*8     dsum_U2mix,dsum_U3mix,dsum_V3mix
      real*8     add1,add2,add3,add4,add5,add6,add7,add8,add9,ad10,ad11
      real*8     uu,vv

      real*8     ds_U1,ds_U2,ds_U3,  ds_V1,ds_V2,ds_V3, ds_UV

!     NURBS coordinates, counters for loops
      integer    p, q, nel, ord(2), ncp(2)
      integer    ispan,jspan
      integer    ic
      integer    lk
      integer    i,j

      integer    fspan

!     Temporary variables
      real*8     tem

! ------------------------------------------------------------------
      p = ord(1)
      q = ord(2)

      allocate ( Nshp(4,p+1),Mshp(4,q+1) )

!     Compute spans for functions
      ispan = fspan(ncp(1),uu,ord(1),kno(:,1))
      jspan = fspan(ncp(2),vv,ord(2),kno(:,2))

!     Evaluate 1D shape functions and derivatives each direction
      call dbsfuns(ispan,p,ncp(1),uu,kno(:,1),3,Nshp)  ! U-direction
      call dbsfuns(jspan,q,ncp(2),vv,kno(:,2),3,Mshp)  ! V-direction

!     Form basis functions and derivatives dR/du and dR/dv
      denom_sum  = 0.0d0
      dsum_U1    = 0.0d0
      dsum_U2    = 0.0d0
      dsum_U3    = 0.0d0
      dsum_V1    = 0.0d0
      dsum_V2    = 0.0d0
      dsum_V3    = 0.0d0
      dsum_U2mix = 0.0d0
      dsum_U3mix = 0.0d0
      dsum_V3mix = 0.0d0

      ic = 0
      do j = 1,q + 1
        do i = 1,p + 1

           ic = ic + 1

           !Basis functions
           shp(10,ic) = Nshp(1,i)*Mshp(1,j)*wt(ic)
           denom_sum  = denom_sum + shp(10,ic)

           !First Derivative
           shp(1,ic) = Nshp(2,i)*Mshp(1,j)*wt(ic)
           dsum_U1   = dsum_U1 + shp(1,ic)

           shp(2,ic) = Nshp(1,i)*Mshp(2,j)*wt(ic)
           dsum_V1   = dsum_V1 + shp(2,ic)

           !Second Derivative
           shp(3,ic) = Nshp(3,i)*Mshp(1,j)*wt(ic)
           dsum_U2   = dsum_U2 + shp(3,ic)

           shp(4,ic) = Nshp(1,i)*Mshp(3,j)*wt(ic)
           dsum_V2   = dsum_V2 + shp(4,ic)

           !Third Derivative
           shp(5,ic) = Nshp(4,i)*Mshp(1,j)*wt(ic)
           dsum_U3   = dsum_U3 + shp(5,ic)

           shp(6,ic) = Nshp(1,i)*Mshp(4,j)*wt(ic)
           dsum_V3   = dsum_V3 + shp(6,ic)

           !Mixed Second Derivative
           shp(7,ic)  = Nshp(2,i)*Mshp(2,j)*wt(ic)
           dsum_U2mix = dsum_U2mix + shp(7,ic)

           !Mixed Third Derivative
           shp(8,ic)  = Nshp(3,i)*Mshp(2,j)*wt(ic)
           dsum_U3mix = dsum_U3mix + shp(8,ic)

           shp(9,ic)  = Nshp(2,i)*Mshp(3,j)*wt(ic)
           dsum_V3mix = dsum_V3mix + shp(9,ic)

        enddo ! i
      enddo ! j

      !Divide through by denominator

      tem = 1.0d0/denom_sum
      ds_U1 = dsum_U1*tem
      ds_U2 = dsum_U2*tem
      ds_U3 = dsum_U3*tem
      ds_V1 = dsum_V1*tem
      ds_V2 = dsum_V2*tem
      ds_V3 = dsum_V3*tem

      ds_UV = dsum_U2mix*tem

      !Basis functions & First Derivatives
      do ic = 1,nel
         shp(10,ic) = shp(10,ic)*tem
         shp( 1,ic) = (shp( 1,ic) - shp(10,ic)*ds_U1)*tem
         shp( 2,ic) = (shp( 2,ic) - shp(10,ic)*ds_V1)*tem
      enddo ! ic

      !Second derivatives
      do ic = 1,nel
         ! Second Derivative in u direction
         shp(3,ic) = (shp(3,ic) - 2.d0*shp(1,ic)*ds_U1
     &             + shp(10,ic)*(2.d0*ds_U1*ds_U1 - ds_U2))*tem

         ! Second Derivative in v direction
         shp(4,ic) = (shp(4,ic) - 2.d0*shp(2,ic)*ds_V1
     &             + shp(10,ic)*(2.d0*ds_V1*ds_V1 - ds_V2))*tem

         !Mixed Second Derivative
         shp(7,ic) = (shp(7,ic) - (shp(1,ic)*ds_V1+shp(2,ic)*ds_U1)
     &             + shp(10,ic)*(2.d0*ds_U1*ds_V1 - ds_UV))*tem
      enddo ! ic

      !Third derivatives
      do ic = 1,nel
         add1 =   shp( 5,ic)* tem
         add2 = 3*shp( 3,ic)* dsum_U1        *tem**2
         add3 = 3*shp( 1,ic)* dsum_U2        *tem**2
         add4 = 6*shp( 1,ic)*(dsum_U1**2)    *tem**3
         add5 =   shp(10,ic)* dsum_U3        *tem**2
         add6 = 6*shp(10,ic)* dsum_U2*dsum_U1*tem**3
         add7 = 6*shp(10,ic)*(dsum_U1**3)    *tem**4
         shp( 5,ic) = add1 - add2 - add3 + add4 - add5 + add6 - add7

         add1 =   shp( 6,ic)* tem
         add2 = 3*shp( 4,ic)* dsum_V1        *tem**2
         add3 = 3*shp( 2,ic)* dsum_V2        *tem**2
         add4 = 6*shp( 2,ic)*(dsum_V1**2)    *tem**3
         add5 =   shp(10,ic)* dsum_V3        *tem**2
         add6 = 6*shp(10,ic)* dsum_V2*dsum_V1*tem**3
         add7 = 6*shp(10,ic)*(dsum_V1**3)    *tem**4
         shp( 6,ic) = add1 - add2 - add3 + add4 - add5 + add6 - add7
      enddo ! ic

      !Mixed Third Derivative
      do ic = 1,nel
         add1 =   shp( 8,ic)* tem
         add2 = 2*shp( 7,ic)* dsum_U1            *tem**2
         add3 = 2*shp( 1,ic)* dsum_U2mix         *tem**2
         add4 =   shp( 3,ic)* dsum_V1            *tem**2
         add5 =   shp( 2,ic)* dsum_U2            *tem**2
         add6 =   shp(10,ic)* dsum_U3mix         *tem**2
         add7 = 4*shp( 1,ic)* dsum_V1    *dsum_U1*tem**3
         add8 = 4*shp(10,ic)* dsum_U2mix *dsum_U1*tem**3
         add9 = 2*shp( 2,ic)*(dsum_U1**2)        *tem**3
         ad10 = 2*shp(10,ic)* dsum_U2    *dsum_V1*tem**3
         ad11 = 6*shp(10,ic)*(dsum_U1**2)*dsum_V1*tem**4
         shp( 8,ic) = add1 - add2 - add3 - add4 - add5 - add6 + add7 +
     &                add8 + add9 + ad10 - ad11

         add1 =   shp( 9,ic)* tem
         add2 = 2*shp( 7,ic)* dsum_V1            *tem**2
         add3 = 2*shp( 2,ic)* dsum_U2mix         *tem**2
         add4 =   shp( 4,ic)* dsum_U1            *tem**2
         add5 =   shp( 1,ic)* dsum_V2            *tem**2
         add6 =   shp(10,ic)* dsum_V3mix         *tem**2
         add7 = 4*shp( 2,ic)* dsum_U1    *dsum_V1*tem**3
         add8 = 4*shp(10,ic)* dsum_U2mix *dsum_U1*tem**3
         add9 = 2*shp( 1,ic)*(dsum_V1**2)        *tem**3
         ad10 = 2*shp(10,ic)* dsum_V2    *dsum_U1*tem**3
         ad11 = 6*shp(10,ic)*(dsum_V1**2)*dsum_U1*tem**4
         shp( 9,ic) = add1 - add2 - add3 - add4 - add5 - add6 + add7 +
     &                add8 + add9 + ad10 - ad11
      enddo ! ic

      deallocate ( Nshp,Mshp )

      end
