!$Id:$
      subroutine nurb_sh1(u,is,knot,p,len,xl,wt,shp,xjac,dx,ndm)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute shape functions for 1-d nurbs
!               and their derivatives up to order 3

!      Reference: Section 4.3, Piegl & Tiller, The NURBS Book

!      Output:
!         shp(2,*) -- Shape functions: 1 = der-1; 2 = shp function.
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'cnurb.h'
      include   'iofile.h'

!     --------------Variable Declarations-----------------------------
      integer (kind=4) :: is,p,len,ndm
      real    (kind=8) :: xjac
      real    (kind=8) :: xl(ndm,*), wt(*),knot(*), shp(2,*)

!     1D nonrational basis functions and derivs in u and v
      real    (kind=8) :: Nshp(2,20)

!     u coordinate at integration point, denominator and derivative sums
      real    (kind=8) :: u, denom_sum, der_sum_1

!     NURBS coordinates, counters for loops
      integer (kind=4) :: ic,ii,jj

!     Temporary variables
      real    (kind=8) :: tem, dx(3)

      real    (kind=8) :: tol

      data       tol / 1.d-10 /
! ------------------------------------------------------------------

!     Set NURBS coordinate data

      if(p.gt.19) then
        write(iow,*) ' Number of nodes on element too large'
        call plstop(.true.)
      endif

!     Evaluate 1D shape functions and derivatives each direction

      jj = is - 1
      call dersbasisfuns(jj, u, p, 1, knot, Nshp)  ! U-direction

!     Form basis functions and derivatives dR/du and dR/dv

      denom_sum = 0.0d0
      der_sum_1 = 0.0d0

      do ic = 1,p+1
        do ii = 1,2
          shp(ii,ic) = 0.0d0
        end do ! ii
      end do ! ic

      do ic = 1,p+1

!       First Derivative

        shp(1,ic) = Nshp(2,ic) * wt(ic)
        der_sum_1 = der_sum_1 + shp(1,ic)

!       Basis functions

        shp(2,ic) = Nshp(1,ic) * wt(ic)
        denom_sum = denom_sum + shp(2,ic)

      enddo ! ic

!     Divide through by denominator

      tem  = 1.0d0/denom_sum
      do ic = 1,p+1
        shp(2,ic) =  shp(2,ic) * tem
        shp(1,ic) = (shp(1,ic) - shp(2,ic)*der_sum_1)*tem
      end do ! ic

!     Calculate dx/dxi

      if(ndm.eq.1) then
        xjac = 0.0d0
        do ic = 1, p+1
          xjac = xjac + xl(1,ic)*shp(1,ic)
        end do ! i
      else ! for multi-dimensional problems
        dx(3) = 0.0d0  ! for 2-d result
        do ii = 1,ndm
          dx(ii) = 0.0d0
          do ic = 1, p+1
            dx(ii) = dx(ii) + xl(ii,ic)*shp(1,ic)
          end do ! ic
        end do ! ii
        xjac = sqrt(dx(1)**2 + dx(2)**2 + dx(3)**2)
      endif

!     Compute inverse of deformation gradient

      if(abs(xjac).gt.tol) then
        tem = 1.0d0/xjac
      else
        tem = 0.0d0
        write(iow,*) ' NURBSH_1: Zero Jacobian:',xjac
        if(xjac.lt.0.0d0) then
          write(*,*) ' NURBSH_1: Zero Jacobian:',xjac
        endif
      endif

!     Compute global derivatives

      do ic = 1, p+1
        shp(1,ic) = shp(1,ic) * tem
      end do ! ic

      xjac = abs(xjac)*(knot(len)-knot(1))*0.5d0

!     Compute tangent unit vector

      do ic = 1,ndm
        dx(ic) = dx(ic) * tem
      end do ! ic

      end subroutine nurb_sh1
