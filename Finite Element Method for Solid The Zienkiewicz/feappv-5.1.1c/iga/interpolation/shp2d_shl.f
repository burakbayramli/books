!$Id:$
      subroutine shp2d_shl(sg,xl,wt,shp,xjac, lknot,ktnum,knots,
     &                     ndm, flag)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute shape functions and derivatives for 2-d nurbs

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

!     --------------VARIABLE DECLARATIONS-----------------------------
      logical          :: flag
      integer          :: ndm
      real    (kind=8) :: xjac
      integer          :: lknot(0:4,*), ktnum(6,*)
      real    (kind=8) :: sg(2),xl(ndm,nel), wt(nel), shp(0:5,nel)
      real    (kind=8) :: knots(dknotig,*)

!     1D nonrational basis functions and derivs in u and v
      real    (kind=8) :: Nshp(4,300), Mshp(4,300)

!     u & v coordinates of integration point, denominator & derivs sums
      real    (kind=8) :: u, v, denom_sum(0:5)

!     NURBS coordinates, counters for loops
      integer          :: i, j, k, ic, p, q, nb
      integer          :: i1(4),ii, j1(4),jj, k1,k2, is
      integer          :: findspan,findsegm

!     Temporary variables
      real    (kind=8) :: tem(0:5)
      real    (kind=8) :: dx2(3,3), dxx(3,2), dr(3), det

      real    (kind=8) :: tol
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

!     Evaluate 1D shape functions and derivatives each direction

      u = (knots(i1(2),k1) + knots(i1(1),k1))*0.5d0
      ii = findspan(i1(4)+p-1,   u, p,    knots(1,k1))
      is = findsegm(u, knots(1,k1), lknot(1,k1) )
      point = np(289) + mr(np(273)+k1-1)
!     call derbezier1d(sg(1),hr(point), p, ii-p+1, Nshp)  ! U-direction
      call derbezier1d(sg(1),hr(point), p, is, Nshp)  ! U-direction

      v = (knots(j1(2),k2) + knots(j1(1),k2))*0.5d0
      jj = findspan(j1(4)+q-1,   v, q,    knots(1,k2))
      is = findsegm(v, knots(1,k2), lknot(1,k2) )
      point = np(289) + mr(np(273)+k2-1)
!     call derbezier1d(sg(2),hr(point), q, jj-q+1, Mshp)  ! V-direction
      call derbezier1d(sg(2),hr(point), q, is, Mshp)  ! V-direction

!     Form basis functions and derivatives dR/du and dR/dv

      do ii = 0,5
        denom_sum(ii) = 0.0d0
      end do ! ii
      ic        = 0
      do j = 0,q
        do i = 0,p
          ic = ic+1

!         Basis functions

          shp(0,ic)    = Nshp(1,i+1)*Mshp(1,j+1) * wt(ic)
          denom_sum(0) = denom_sum(0) + shp(0,ic)

!         First derivatives

          shp(1,ic)    = Nshp(2,i+1)*Mshp(1,j+1) * wt(ic)
          denom_sum(1) = denom_sum(1) + shp(1,ic)

          shp(2,ic)    = Nshp(1,i+1)*Mshp(2,j+1) * wt(ic)
          denom_sum(2) = denom_sum(2) + shp(2,ic)

!         Second derivatives

          shp(3,ic)    = Nshp(3,i+1)*Mshp(1,j+1) * wt(ic)
          denom_sum(3) = denom_sum(3) + shp(3,ic)

          shp(4,ic)    = Nshp(1,i+1)*Mshp(3,j+1) * wt(ic)
          denom_sum(4) = denom_sum(4) + shp(4,ic)

          shp(5,ic)    = Nshp(2,i+1)*Mshp(2,j+1) * wt(ic)
          denom_sum(5) = denom_sum(5) + shp(5,ic)
        enddo ! i
      enddo ! j

!     Compute rational factors

      tem(0)  = 1.0d0/denom_sum(0)
      do k = 1,5
        tem(k) = denom_sum(k)*tem(0)
      end do ! k

!     Compute second derivatives of rational shape functions

      do i = 1,nel
        shp(3,i) = (shp(3,i) - 2.0d0*tem(1)*shp(1,i)
     &           + (2.d0*tem(1)*tem(1) - tem(3))*shp(0,i))*tem(0)
        shp(4,i) = (shp(4,i) - 2.0d0*tem(2)*shp(2,i)
     &           + (2.d0*tem(2)*tem(2) - tem(4))*shp(0,i))*tem(0)
        shp(5,i) = (shp(5,i) - tem(1)*shp(2,i) - tem(2)*shp(1,i)
     &           + (2.d0*tem(1)*tem(2) - tem(5))*shp(0,i))*tem(0)
      end do ! i

!     Compute first derivatives of rational shape functions

      do i = 1,nel
        shp(1,i) = (shp(1,i) - shp(0,i)*tem(1))*tem(0)
        shp(2,i) = (shp(2,i) - shp(0,i)*tem(2))*tem(0)
      end do ! i

!     Compute rational shape functions

      do i = 1,nel
        shp(0,i) =  shp(0,i) * tem(0)
      end do ! i

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
          tem(0) = 1.0d0/xjac
        else
          tem(0) = 0.0d0
          write(iow,*) ' SHP2D_SHL: Zero Jacobian:',xjac
          if(xjac.lt.0.0d0) then
            write(*,*) ' SHP2D_SHL: Zero Jacobian:',xjac
          endif
        endif
        dxidx(1,1) =  dxidx(1,1) * tem(0)
        dxidx(1,2) =  dxidx(1,2) * tem(0)
        dxidx(2,1) = -dxdxi(2,1) * tem(0)
        dxidx(2,2) =  dxdxi(1,1) * tem(0)
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
          tem(1)   = shp(1,i) * dxidx(1,1) + shp(2,i) * dxidx(2,1)
          shp(2,i) = shp(1,i) * dxidx(1,2) + shp(2,i) * dxidx(2,2)
          shp(1,i) = tem(1)
        end do ! i

!       Compute second derivatives

        dx2(1,1) = dxdxi(1,1)**2
        dx2(1,2) = dxdxi(2,1)**2
        dx2(1,3) = dxdxi(1,1)*dxdxi(2,1)*2.0d0

        dx2(2,1) = dxdxi(1,2)**2
        dx2(2,2) = dxdxi(2,2)**2
        dx2(2,3) = dxdxi(1,2)*dxdxi(2,2)*2.0d0

        dx2(3,1) = dxdxi(1,1)*dxdxi(1,2)
        dx2(3,2) = dxdxi(2,1)*dxdxi(2,2)
        dx2(3,3) = dxdxi(1,1)*dxdxi(2,2) + dxdxi(2,1)*dxdxi(1,2)

        call invert3(dx2,det)

        do k = 1,3
          dxx(k,1) = 0.0d0
          dxx(k,2) = 0.0d0
          do i = 1,nel
            dxx(k,1) = dxx(k,1) + shp(k+2,i)*xl(1,i)
            dxx(k,2) = dxx(k,2) + shp(k+2,i)*xl(2,i)
          end do ! i
        end do ! k

!       Compute global derivatives

        do i = 1,nel
          do k = 1,3
            dr(k) = shp(k+2,i) - dxx(k,1)*shp(1,i) - dxx(k,2)*shp(2,i)
          end do ! k

          do k = 1,3
            shp(k+2,i) = dx2(k,1)*dr(1)
     &                 + dx2(k,2)*dr(2)
     &                 + dx2(k,3)*dr(3)
          end do ! k
        end do ! i
      endif ! .not.flag

!     Return absolute value of jacobian

      xjac = abs(xjac)

      end subroutine shp2d_shl
