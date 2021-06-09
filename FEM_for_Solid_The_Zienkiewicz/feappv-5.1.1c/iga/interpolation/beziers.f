!$Id:$
      subroutine beziers(xi, p, shp)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute Bernstein polynomials for use in Bezier shape
!               functions and derivatives w/r xi

!      Compute: df/dxi = df/dt * dt/dxi ; etc.

!      Inputs:
!         xi         - Gauss point ( -1 < xi < 1)
!         p          - Order of functions desired

!      Outputs:
!         shp(*)     - Bernstein shape functions
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      integer (kind=4) :: p, i
      real    (kind=8) :: xi,shp(*), t, t1, bpi, tii,ti1, tpp,tp1

      real    (kind=8) :: binom

!     Set coordinate on interval (0 < t < 1)

      t  = 0.5d0 + 0.5d0*xi
      t1 = 1.d0 - t

!     Select order to compute

      select case (p)

      case (1)

        shp(1) =  t1
        shp(2) =  t

      case (2)

        shp(1) =  t1**2
        shp(2) =  2.d0*t*t1
        shp(3) =  t**2

      case (3)

        shp(1) =  t1**3
        shp(2) =  3.d0*t*(t1**2)
        shp(3) =  3.d0*(t**2)*t1
        shp(4) =  t**3

      case (4)

        shp(1) =  t1**4
        shp(2) =  4.d0*t*(t1**3)
        shp(3) =  6.d0*(t**2)*(t1)**2
        shp(4) =  4.d0*(t**3)*(t1)
        shp(5) =  t**4

      case (5)

        shp(1) =  t1**5
        shp(2) =  5.d0*t*(t1**4)
        shp(3) = 10.d0*(t**2)*(t1**3)
        shp(4) = 10.d0*(t**3)*(t1**2)
        shp(5) =  5.d0*(t**4)*t1
        shp(6) =  t**5

      case default   ! p > 5

        shp(  1)   =  t1**p
        shp(p+1)   =  t**p
        do i = 1,p-1
          bpi      =  binom(p,i)
          ti1      =  t**(i-1)
          tii      =  t*ti1
          tp1      =  t1**(p-i-1)
          tpp      =  t1*tp1
          shp(i+1) =  bpi*tii*tpp
        end do ! i

      end select

      end subroutine beziers
