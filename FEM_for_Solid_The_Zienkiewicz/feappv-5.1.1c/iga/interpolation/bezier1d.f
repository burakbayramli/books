!$Id:$
      subroutine bezier1d(xi, p, shp)

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
!         shp(0:2,*) - Bernstein shape functions and derivatives
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      integer (kind=4) :: p, i
      real    (kind=8) :: xi, t, t1, bpi, tii,ti1,ti2, tpp,tp1,tp2
      real    (kind=8) :: shp(0:2,*)

      real    (kind=8) :: binom

!     Set coordinate on interval (0 < t < 1)

      t  = 0.5d0 + 0.5d0*xi
      t1 = 1.d0 - t

!     Select order to compute

      select case (p)

      case (1)

        shp(0,1) =  t1
        shp(0,2) =  t

        shp(1,1) = -0.5d0
        shp(1,2) =  0.5d0

        shp(2,1) =  0.0d0
        shp(2,2) =  0.0d0

      case (2)

        shp(0,1) =  t1**2
        shp(0,2) =  2.d0*t*t1
        shp(0,3) =  t**2

        shp(1,1) = -t1
        shp(1,2) =  1.0d0 - 2.d0*t
        shp(1,3) =  t

        shp(2,1) =  0.5d0
        shp(2,2) = -1.0d0
        shp(2,3) =  0.5d0

      case (3)

        shp(0,1) =  t1**3
        shp(0,2) =  3.d0*t*(t1**2)
        shp(0,3) =  3.d0*(t**2)*t1
        shp(0,4) =  t**3

        shp(1,1) = -1.5d0*t1**2
        shp(1,2) =  1.5d0*t1**2 -  3.0d0*t*t1
        shp(1,3) =  3.0d0*t*t1  -  1.5d0*(t**2)
        shp(1,4) =  1.5d0*t**2

        shp(2,1) =  1.5d0*t1
        shp(2,2) = -3.0d0*t1 + 1.5d0*t
        shp(2,3) =  1.5d0*t1 - 3.0d0*t
        shp(2,4) =  1.5d0*t

      case (4)

        shp(0,1) =  t1**4
        shp(0,2) =  4.d0*t*(t1**3)
        shp(0,3) =  6.d0*(t**2)*(t1)**2
        shp(0,4) =  4.d0*(t**3)*(t1)
        shp(0,5) =  t**4

        shp(1,1) = -2.0d0*(t1**3)
        shp(1,2) =  2.0d0*(t1**3)   - 6.0d0*t*(t1**2)
        shp(1,3) =  6.0d0*t*(t1**2) - 6.0d0*(t**2)*t1
        shp(1,4) =  6.0d0*(t**2)*t1 - 2.0d0*(t**3)
        shp(1,5) =  2.0d0*t**3

        shp(2,1) =  3.0d0*(t1**2)
        shp(2,2) = -6.0d0*(t1**2) +  6.0d0*t*t1
        shp(2,3) =  3.0d0*(t1**2) - 12.0d0*t*t1 + 3.0d0*(t**2)
        shp(2,4) =  6.0d0*t*t1    -  6.0d0*(t**2)
        shp(2,5) =  3.0d0*(t**2)

      case (5)

        shp(0,1) =  t1**5
        shp(0,2) =  5.d0*t*(t1**4)
        shp(0,3) = 10.d0*(t**2)*(t1**3)
        shp(0,4) = 10.d0*(t**3)*(t1**2)
        shp(0,5) =  5.d0*(t**4)*t1
        shp(0,6) =  t**5

        shp(1,1) = -2.5d0*(t1**4)
        shp(1,2) =  2.5d0*(t1**4)        - 10.0d0*t*(t1**3)
        shp(1,3) = 10.0d0*t*(t1**3)      - 15.0d0*(t**2)*(t1**2)
        shp(1,4) = 15.0d0*(t**2)*(t1**2) - 10.0d0*(t**3)*t1
        shp(1,5) = 10.0d0*(t**3)*t1      -  2.5d0*(t**4)
        shp(1,6) =  2.5d0*(t**4)

        shp(2,1) =  5.0d0*(t1**3)
        shp(2,2) =-10.0d0*(t1**3)   + 15.0d0*t*(t1**2)
        shp(2,3) =  5.0d0*(t1**3)   - 30.0d0*t*(t1**2) -15.0d0*(t**2)*t1
        shp(2,4) = 15.0d0*t*(t1**2) - 30.0d0*(t**2)*t1 + 5.0d0*(t**3)
        shp(2,5) = 15.0d0*(t**2)*t1 - 10.0d0*(t**3)
        shp(2,6) =  5.0d0*(t**3)

      case default   ! p > 5

        shp(0,  1)   =  t1**p
        shp(0,p+1)   =  t**p
        shp(1,  1)   = -dble(p)*t1**(p-1)*0.5d0
        shp(1,p+1)   =  dble(p)*t**(p-1)*0.5d0
        do i = 1,p-1
          bpi        =  binom(p,i)
          ti1        =  t**(i-1)
          tii        =  t*ti1
          tp1        =  t1**(p-i-1)
          tpp        =  t1*tp1
          shp(0,i+1) =  bpi*tii*tpp
          shp(1,i+1) = (bpi*(dble(i)*ti1*tpp - dble(p-i)*tii*tp1))*0.5d0
        end do ! i

        shp(2,  1)   =  dble(p*p-p)*t1**(p-2)*0.25d0
        shp(2,  2)   = (dble(p*(p-1)*(p-2))*t*t1**(p-3)
     &               -  dble(p*(p-1))*t1**(p-2))*0.25d0
        shp(2,p  )   = (dble(p*(p-1)*(p-2))*t**(p-3)*t1
     &               -  dble(p*(p-1))*t**(p-2))*0.25d0
        shp(2,p+1)   =  dble(p*p-p)*t**(p-2)*0.25d0
        do i = 2,p-2
          bpi        =  binom(p,i)
          ti2        =  t**(i-2)
          ti1        =  t*ti2
          tii        =  t*ti1
          tp2        =  t1**(p-i-2)
          tp1        =  t1*tp2
          tpp        =  t1*tp1
          shp(2,i+1) = (bpi*(dble(i*i-i)*ti2*tpp
     &               -  dble(2*i*(p-i))*ti1*tp1
     &               +  dble((p-i)*(p-i-1))*tii*tp2))*0.25d0
        end do ! i

      end select

      end subroutine bezier1d
