!$Id:$
      subroutine shp3d_nurb(sg,xl,wt,shp,shpl,xjac, lknot,ktnum,knots,
     &                      ndm)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute shape functions for 3-d nurbs

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
      integer (kind=4) :: ndm

      real    (kind=8) :: xjac
      integer (kind=4) :: lknot(0:4,*), ktnum(6,*)
      real    (kind=8) :: sg(3),xl(ndm,nel), wt(nel)
      real    (kind=8) :: shp(4,nel), shpl(nel)
      real    (kind=8) :: knots(dknotig,*)

!     1D nonrational basis functions and derivs in u and v
!     real    (kind=8) :: Nshp(2,300), Mshp(2,300), Oshp(2,300)
      real    (kind=8) :: Nshp(4,300), Mshp(4,300), Oshp(4,300)
      real    (kind=8) :: Ishp(300)  , Jshp(300)  , Kshp(300)

!     u,v and w coords of integration point, denominator & deriv sums
      real    (kind=8) :: u, v, w
      real    (kind=8) :: denom_sum, der_sum_U, der_sum_V, der_sum_W
!     real    (kind=8) :: du,dv,dw

!     NURBS coordinates, counters for loops
      integer (kind=4) :: i, j, k, ic, p, q, r, is, js, ks
      integer (kind=4) :: i1(4),ii, j1(4),jj, k1(4),kk, nb
      integer (kind=4) :: l1,l2,l3

      integer (kind=4) :: findsegm

!     temporary variables
      real    (kind=8) :: tem, temU, temV, temW, ctem(3)

      real    (kind=8) :: tol

      data       tol / 1.d-10 /
! ------------------------------------------------------------------

!     Set NURBS coordinate data

      nb = mod(eltyp,500)
      ii = eltyp/500
      jj = elty2
      kk = elty3

      l1 = 4*kdiv*(nb-1)
      call pknotdiv(mr(np(300)+l1), ii, i1)
      call pknotdiv(mr(np(300)+l1), jj, j1)
      call pknotdiv(mr(np(300)+l1), kk, k1)

!     Order of polynomials

      l1 = ktnum(1,nb)
      l2 = ktnum(2,nb)
      l3 = ktnum(3,nb)
      p  = lknot(2,l1)
      q  = lknot(2,l2)
      r  = lknot(2,l3)

!     Set element u, v and w coordinates of integration point

      u = ((knots(i1(2),l1) - knots(i1(1),l1))*sg(1)
     &    + knots(i1(2),l1) + knots(i1(1),l1))*0.5d0

      v = ((knots(j1(2),l2) - knots(j1(1),l2))*sg(2)
     &    + knots(j1(2),l2) + knots(j1(1),l2))*0.5d0

      w = ((knots(k1(2),l3) - knots(k1(1),l3))*sg(3)
     &    + knots(k1(2),l3) + knots(k1(1),l3))*0.5d0

!     Evaluate 1D shape functions and derivatives in each direction

      is = findsegm(u, knots(1,l1), lknot(1,l1) )
      point = np(289) + mr(np(273)+l1-1)
      call derbezier1d(sg(1),hr(point), p, is, Nshp)  ! U-direction

      js = findsegm(v, knots(1,l2), lknot(1,l2) )
      point = np(289) + mr(np(273)+l2-1)
      call derbezier1d(sg(2),hr(point), q, js, Mshp)  ! V-direction

      ks = findsegm(w, knots(1,l3), lknot(1,l3) )
      point = np(289) + mr(np(273)+l3-1)
      call derbezier1d(sg(3),hr(point), r, ks, Oshp)  ! W-direction

!     Form basis functions and derivatives dR/du and dR/dv

      denom_sum = 0.0d0
      der_sum_U = 0.0d0
      der_sum_V = 0.0d0
      der_sum_W = 0.0d0
      ic        = 0
      do j = 0,q
        do i = 0,p
          do k = 0,r
            ic = ic+1

!           Basis functions

            shp(4,ic) = Nshp(1,i+1)*Mshp(1,j+1)*Oshp(1,k+1) * wt(ic)
            denom_sum = denom_sum + shp(4,ic)

!           Derivatives

            shp(1,ic) = Nshp(2,i+1)*Mshp(1,j+1)*Oshp(1,k+1) * wt(ic)
            der_sum_U = der_sum_U + shp(1,ic)

            shp(2,ic) = Nshp(1,i+1)*Mshp(2,j+1)*Oshp(1,k+1) * wt(ic)
            der_sum_V = der_sum_V + shp(2,ic)

            shp(3,ic) = Nshp(1,i+1)*Mshp(1,j+1)*Oshp(2,k+1) * wt(ic)
            der_sum_W = der_sum_W + shp(3,ic)

          enddo ! k
        enddo ! i
      enddo ! j

!     Divide through by denominator

      tem  = 1.0d0/denom_sum
      temU = der_sum_U*tem
      temV = der_sum_V*tem
      temW = der_sum_W*tem
      do i = 1,nel
        shp(1,i) = (shp(1,i) - shp(4,i)*temU)*tem
        shp(2,i) = (shp(2,i) - shp(4,i)*temV)*tem
        shp(3,i) = (shp(3,i) - shp(4,i)*temW)*tem
        shp(4,i) =  shp(4,i) * tem
      enddo ! i

!     Calculate dx/dxi

      do j = 1,3
        do i = 1,3
          dxdxi(i,j) = 0.0d0
          do ic = 1,nel
            dxdxi(i,j) = dxdxi(i,j) + xl(i,ic)*shp(j,ic)
          end do ! ic
        end do ! i
      end do ! j

!     Compute inverse of deformation gradient

      dxidx(1,1) =  dxdxi(2,2)*dxdxi(3,3) - dxdxi(2,3)*dxdxi(3,2)
      dxidx(1,2) =  dxdxi(3,2)*dxdxi(1,3) - dxdxi(3,3)*dxdxi(1,2)
      dxidx(1,3) =  dxdxi(1,2)*dxdxi(2,3) - dxdxi(1,3)*dxdxi(2,2)

      dxidx(2,1) =  dxdxi(2,3)*dxdxi(3,1) - dxdxi(2,1)*dxdxi(3,3)
      dxidx(2,2) =  dxdxi(3,3)*dxdxi(1,1) - dxdxi(3,1)*dxdxi(1,3)
      dxidx(2,3) =  dxdxi(1,3)*dxdxi(2,1) - dxdxi(1,1)*dxdxi(2,3)

      dxidx(3,1) =  dxdxi(2,1)*dxdxi(3,2) - dxdxi(2,2)*dxdxi(3,1)
      dxidx(3,2) =  dxdxi(3,1)*dxdxi(1,2) - dxdxi(3,2)*dxdxi(1,1)
      dxidx(3,3) =  dxdxi(1,1)*dxdxi(2,2) - dxdxi(1,2)*dxdxi(2,1)

      xjac       =  dxidx(1,1) * dxdxi(1,1) ! Note xjac in common
     &           +  dxidx(1,2) * dxdxi(2,1)
     &           +  dxidx(1,3) * dxdxi(3,1)

      if(abs(xjac).gt.tol*hsize(2)) then
        tem = 1.0d0/xjac
      else
        tem = 0.0d0
        write(iow,*) ' SHP3D_NURB: Zero Jacobian:',xjac
        if(xjac.lt.0.0d0) then
          write(*,*) ' SHP3D_NURB: Zero Jacobian:',xjac
        endif
      endif
      do j = 1,3
        do i = 1,3
          dxidx(i,j) =  dxidx(i,j) * tem
        end do ! i
      end do ! j

!     Compute global derivatives

      do i = 1, nel
        do j = 1,3
          ctem(j) = shp(1,i) * dxidx(1,j)
     &            + shp(2,i) * dxidx(2,j)
     &            + shp(3,i) * dxidx(3,j)
        end do ! j
        do j = 1,3
          shp(j,i) = ctem(j)
        end do ! j
      end do ! i

!     Mixed element shape function: F_bar formulation

      point = np(335) + mr(np(334)+l1-1)
      call mixbezier1d(sg(1),hr(point), p-1, is, Ishp)  ! U-direction

      point = np(335) + mr(np(334)+l2-1)
      call mixbezier1d(sg(2),hr(point), q-1, js, Jshp)  ! V-direction

      point = np(335) + mr(np(334)+l3-1)
      call mixbezier1d(sg(3),hr(point), r-1, ks, Kshp)  ! W-direction

!     call basisfuns(ii-1, u, p-1, knots(2,l1), Ishp)  ! U-direction
!     call basisfuns(jj-1, v, q-1, knots(2,l2), Jshp)  ! V-direction
!     call basisfuns(kk-1, w, r-1, knots(2,l3), Kshp)  ! W-direction

      ic = 0
      do j = 1,q+1
        do i = 1,p+1
          do k = 1,r+1
            ic = ic+1

!           Basis functions

            if(i.lt.p+1 .and. j.lt.q+1 .and. k.lt.r+1) then
              shpl(ic) = Ishp(i)*Jshp(j)*Kshp(k)
            else
              shpl(ic) = 0.0d0
            endif

          end do ! k
        end do ! i
      end do ! j

      xjac = abs(xjac)

      end subroutine shp3d_nurb
