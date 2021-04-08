!$Id:$
      subroutine shp2d_nurb(sg,xl,wt,shp,shpl,xjac, lknot,ktnum,knots,
     &                      ndm, flag)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute shape functions for 2-d nurbs

!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'debugs.h'
      include   'cnurb.h'
      include   'eldata.h'
      include   'iofile.h'
      include   'igdata.h'
      include   'qudshp.h'
      include   'p_point.h'

      include   'pointer.h'
      include   'comblk.h'

!     --------------VARIABLE DECLARATIONS-----------------------------
      logical          :: flag
      integer (kind=4) :: ndm
      real    (kind=8) :: xjac
      integer (kind=4) :: lknot(0:4,*), ktnum(6,*)
      real    (kind=8) :: sg(2),xl(ndm,nel), wt(nel)
      real    (kind=8) :: shp(3,nel), shpl(*)
      real    (kind=8) :: knots(dknotig,*)

!     1D nonrational basis functions and derivs in u and v
      real    (kind=8) :: Nshp(4,300), Mshp(4,300)
      real    (kind=8) :: Ishp(300), Jshp(300)

!     u & v coordinates of integration point, denominator & deriv sums
      real    (kind=8) :: u, v, denom_sum, der_sum_U, der_sum_V

!     NURBS coordinates, counters for loops
      integer (kind=4) :: i, j, k, ic, p, q, nb
      integer (kind=4) :: i1(4),ii, j1(4),jj, k1,k2, is, js
      integer (kind=4) :: findsegm

!     Temporary variables
      integer (kind=4) :: pp , qq
      real    (kind=8) :: tem, temU, temV

      real   (kind=8) ::  tol
      data       tol / 1.d-10 /
! ------------------------------------------------------------------
!     Set NURBS coordinate data

      if(nel.gt.64) then
        write(iow,*) ' Number of nodes on element too large'
        call plstop(.true.)
      endif

      nb = mod(eltyp,500)
      ii = eltyp/500
      jj = elty2

      k1 = 4*kdiv*(nb - 1)
      call pknotdiv(mr(np(300)+k1), ii, i1)
      call pknotdiv(mr(np(300)+k1), jj, j1)

!     Order of polynomials

      k1 = ktnum(1,nb)
      k2 = ktnum(2,nb)
      p  = lknot(2,k1)
      q  = lknot(2,k2)
      pp = p + 1
      qq = q + 1

!     Evaluate 1D Bernstein shape functions and derivatives each direction

      u  = (knots(i1(2),k1) + knots(i1(1),k1))*0.5d0
      is = findsegm(u, knots(1,k1), lknot(1,k1) )
      point = np(289) + mr(np(273)+k1-1)
      call derbezier1d(sg(1),hr(point), p, is, Nshp)  ! U-direction

      v  = (knots(j1(2),k2) + knots(j1(1),k2))*0.5d0
      js = findsegm(v, knots(1,k2), lknot(1,k2) )
      point = np(289) + mr(np(273)+k2-1)
      call derbezier1d(sg(2),hr(point), q, js, Mshp)  ! V-direction

!     Form basis functions and derivatives dR/du and dR/dv

      denom_sum = 0.0d0
      der_sum_U = 0.0d0
      der_sum_V = 0.0d0
      ic        = 0
      do j = 0,q
        do i = 0,p
          ic = ic+1

!         Basis functions

          shp(3,ic) = Nshp(1,i+1)*Mshp(1,j+1) * wt(ic)
          denom_sum = denom_sum + shp(3,ic)

!         Derivatives

          shp(1,ic) = Nshp(2,i+1)*Mshp(1,j+1) * wt(ic)
          der_sum_U = der_sum_U + shp(1,ic)

          shp(2,ic) = Nshp(1,i+1)*Mshp(2,j+1) * wt(ic)
          der_sum_V = der_sum_V + shp(2,ic)

        enddo ! i
      enddo ! j

!     Divide through by denominator

      tem  = 1.0d0/denom_sum
      temU = der_sum_U*tem
      temV = der_sum_V*tem
      do i = 1,nel
        shp(1,i) = (shp(1,i) - shp(3,i)*temU)*tem
        shp(2,i) = (shp(2,i) - shp(3,i)*temV)*tem
        shp(3,i) =  shp(3,i) * tem
      enddo ! i

!     Calculate dx/dxi

      do j = 1,2
        do i = 1,ndm
          dxdxi(i,j) = 0.0d0
        end do ! i
      end do ! j

      ic = 0
      do j = 0,q
        do i = 0,p
          ic = ic + 1
          do k = 1,ndm
            dxdxi(k,1) = dxdxi(k,1) + xl(k,ic)*shp(1,ic)
            dxdxi(k,2) = dxdxi(k,2) + xl(k,ic)*shp(2,ic)
          end do ! k
        end do ! i
      end do ! j

!     Compute the inverse of deformation gradient

      if(ndm.eq.2) then
        dxidx(1,1) =  dxdxi(2,2)
        dxidx(1,2) = -dxdxi(1,2)

        xjac       =  dxidx(1,1) * dxdxi(1,1) ! Note xjac in common
     &             +  dxidx(1,2) * dxdxi(2,1)

        if(abs(xjac).gt.tol*hsize(2)) then
          tem = 1.0d0/xjac
        else
          tem = 0.0d0
          write(iow,*) ' SHP2D_NURB: Zero Jacobian:',xjac
          if(xjac.lt.0.0d0) then
            write(*,*) ' SHP2D_NURB: Zero Jacobian:',xjac
          endif
        endif
        dxidx(1,1) =  dxidx(1,1) * tem
        dxidx(1,2) =  dxidx(1,2) * tem
        dxidx(2,1) = -dxdxi(2,1) * tem
        dxidx(2,2) =  dxdxi(1,1) * tem
      elseif(ndm.eq.3) then

!       Compute cross product for surface area use

        dxdxi(1,3) = dxdxi(2,1)*dxdxi(3,2) - dxdxi(2,2)*dxdxi(3,1)
        dxdxi(2,3) = dxdxi(3,1)*dxdxi(1,2) - dxdxi(3,2)*dxdxi(1,1)
        dxdxi(3,3) = dxdxi(1,1)*dxdxi(2,2) - dxdxi(1,2)*dxdxi(2,1)

        xjac       = sqrt(dxdxi(1,3)**2 +dxdxi(2,3)**2 +dxdxi(3,3)**2)

      endif

!     Compute global derivatives

      if(.not.flag) then
        do i = 1, nel
          tem      = shp(1,i) * dxidx(1,1) + shp(2,i) * dxidx(2,1)
          shp(2,i) = shp(1,i) * dxidx(1,2) + shp(2,i) * dxidx(2,2)
          shp(1,i) = tem
        end do ! i
      endif

!     Mixed element shape function: F_bar formulation

      point = np(335) + mr(np(334)+k1-1)
      call mixbezier1d(sg(1),hr(point), p-1, is, Ishp)  ! U-direction

      point = np(335) + mr(np(334)+k2-1)
      call mixbezier1d(sg(2),hr(point), q-1, js, Jshp)  ! V-direction

!     u  = u + sg(1)*(knots(i1(2),k1) - knots(i1(1),k1))*0.5d0
!     call basisfuns(ii-1, u, p-1, knots(2,k1), Ishp)  ! U-direction
!     v  = v + sg(2)*(knots(j1(2),k2) - knots(j1(1),k2))*0.5d0
!     call basisfuns(jj-1, v, q-1, knots(2,k2), Jshp)  ! V-direction

      ic = 0
      do j = 1,qq
        do i = 1,pp
          ic = ic+1

!         Basis functions

          if(i.lt.pp .and. j.lt.qq) then
            shpl(ic) = Ishp(i)*Jshp(j)
          else
            shpl(ic) = 0.0d0
          endif

        end do ! i
      end do ! j

      xjac = abs(xjac)

      end subroutine shp2d_nurb
