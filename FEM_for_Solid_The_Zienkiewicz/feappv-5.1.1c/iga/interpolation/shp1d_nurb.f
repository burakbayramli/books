!$Id:$
      subroutine shp1d_nurb(sg,xl,wt,shp,xjac,lknot,ktnum,knots,
     &                      ndm, flag)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute shape functions for 1-d nurbs
!               and their derivatives up to order 3

!      Reference: Section 4.3, Piegl & Tiller, The NURBS Book

!      Output:
!         shp(4,*) -- Shape functions: 1 = der-1; 2 = der-2; 3 = der-3
!                                      4 = shp function.
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'cnurb.h'
      include   'eldata.h'
      include   'iofile.h'
      include   'igdata.h'
      include   'qudshp.h'
      include   'p_point.h'

      include   'pointer.h'
      include   'comblk.h'

!     --------------Variable Declarations-----------------------------
      logical          :: flag
      integer (kind=4) :: ndm
      real    (kind=8) :: sg, xjac
      integer (kind=4) :: lknot(0:4,*), ktnum(6,*)
      real    (kind=8) :: xl(ndm,nel), wt(nel)
      real    (kind=8) :: shp(4,nel), knots(dknotig,*)

!     1D nonrational basis functions and derivs in u and v
      real    (kind=8) :: Nshp(4,300)

!     u coordinate at integration point, denominator and derivative sums
      real    (kind=8) :: u, denom_sum, der_sum_1, der_sum_2, der_sum_3

!     NURBS coordinates, counters for loops
      integer (kind=4) :: ic, p
      integer (kind=4) :: i1(4),ii,nn, nb, is
      integer (kind=4) :: findsegm

!     Temporary variables
      real    (kind=8) :: tem,tem2

      real    (kind=8) :: tol

      data       tol / 1.d-10 /
! ------------------------------------------------------------------

!     Set NURBS coordinate data

      if(nel.gt.20) then
        write(iow,*) ' Number of nodes on element too large'
        call plstop(.true.)
      endif

      nb = mod(eltyp,500)
      ii = eltyp/500

      nn = 4*kdiv*(nb - 1)
      call pknotdiv(mr(np(300)+nn), ii, i1)

!     Order of polynomials

      nn = ktnum(1,nb)
      p  = lknot(2,nn)

!     Compute shape functions and derivatives using extraction operator

      u     = (knots(i1(2),nn) + knots(i1(1),nn))*0.5d0
      is    = findsegm(u, knots(1,nn), lknot(1,nn) )
      point = np(289) + mr(np(273)+nn-1)            ! Pointer to C_e
      call derbezier1d(sg, hr(point), p, is, Nshp)  ! U-direction

!     Form basis functions and derivatives dR/du and dR/dv

      denom_sum = 0.0d0
      der_sum_1 = 0.0d0
      der_sum_2 = 0.0d0
      der_sum_3 = 0.0d0

      do ic = 1,nel
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
      do ic = 1,nel
        shp(4,ic) =  shp(4,ic) * tem
        shp(1,ic) = (shp(1,ic) - shp(4,ic)*der_sum_1)*tem
      end do ! ic

!     Second derivatives

      if(p.gt.1) then
        do ic = 1,nel
          shp(2,ic) = (shp(2,ic) - 2.0d0*shp(1,ic)*der_sum_1
     &                                 - shp(4,ic)*der_sum_2)*tem
        enddo ! ic
      endif ! p > 1

!     Third derivatives

      if(p.gt.2) then
        do ic = 1,nel
          shp(3,ic) = (shp(3,ic) - 3.0d0*shp(2,ic)*der_sum_1
     &                           - 3.0d0*shp(1,ic)*der_sum_2
     &                                 - shp(4,ic)*der_sum_3)*tem
        enddo ! ic
      endif ! p > 2

!     Calculate dx/dxi

      if(ndm.eq.1) then
        xjac      = 0.0d0
        der_sum_2 = 0.0d0
        der_sum_3 = 0.0d0
        do ic = 1, p+1
          xjac      = xjac      + xl(1,ic)*shp(1,ic)
          der_sum_2 = der_sum_2 + xl(1,ic)*shp(2,ic)
          der_sum_3 = der_sum_3 + xl(1,ic)*shp(3,ic)
        end do ! i
      else ! for multi-dimensional problems
        dxdxi(3,1) = 0.0d0  ! for 2-d result
        do ii = 1,ndm
          dxdxi(ii,1) = 0.0d0
          do ic = 1, p+1
            dxdxi(ii,1) = dxdxi(ii,1) + xl(ii,ic)*shp(1,ic)
          end do ! ic
        end do ! ii
        xjac = sqrt(dxdxi(1,1)**2 + dxdxi(2,1)**2 + dxdxi(3,1)**2)
        if(ndm.eq.2) then
          dxdxi(1,2) =  dxdxi(2,1)
          dxdxi(2,2) = -dxdxi(1,1)
        endif
      endif

!     Compute inverse of deformation gradient

      if(abs(xjac).gt.tol*hsize(2)) then
        tem = 1.0d0/xjac
      else
        tem = 0.0d0
        write(iow,*) ' SHP1D_NURB: Zero Jacobian:',xjac
        if(xjac.lt.0.0d0) then
          write(*,*) ' SHP1D_NURB: Zero Jacobian:',xjac
        endif
      endif

!     Compute global derivatives

      if(.not.flag) then
        do ic = 1, nel
          shp(1,ic) = shp(1,ic) * tem
        end do ! ic
        if(p.gt.1) then
          tem2 = tem*tem
          do ic = 1, nel
            shp(2,ic) = (shp(2,ic)
     &                -  shp(1,ic) * der_sum_2) * tem2
          end do ! ic
          if(p.gt.2) then
            tem2 = tem2*tem
            do ic = 1, nel
              shp(3,ic) = (shp(3,ic)
     &                  -  shp(2,ic) * der_sum_2  * xjac * 3.0d0
     &                  -  shp(1,ic) * der_sum_3) * tem2
            end do ! ic
          endif
        endif
      endif

      xjac = abs(xjac)

      end subroutine shp1d_nurb
