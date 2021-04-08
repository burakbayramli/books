!$Id:$
      subroutine int1dg(l,sw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Gauss quadrature for 1-d element

!      Inputs:
!         l     - Number of points

!      Outputs:
!         sw(1,*) - Gauss points
!         sw(2,*) - Gauss weights
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: l
      real (kind=8) :: sw(2,*), t

      save

      if(l.eq.1) then

        sw(1,1) = 0.0d0
        sw(2,1) = 2.0d0

      elseif(l.eq.2) then

        sw(1,1) = -1.d0/sqrt(3.d0)
        sw(1,2) = -sw(1,1)
        sw(2,1) = 1.0d0
        sw(2,2) = 1.0d0

      elseif(l.eq.3) then

        sw(1,1) = -sqrt(0.6d0)
        sw(1,2) = 0.0d0
        sw(1,3) = -sw(1,1)
        sw(2,1) = 5.d0/9.d0
        sw(2,2) = 8.d0/9.d0
        sw(2,3) = sw(2,1)

      elseif(l.eq.4) then

        t       =  sqrt(4.8d0)
        sw(1,1) = -sqrt((3.d0+t)/7.d0)
        sw(1,2) = -sqrt((3.d0-t)/7.d0)
        sw(1,3) = -sw(1,2)
        sw(1,4) = -sw(1,1)
        t       =  1.d0/3.d0/t
        sw(2,1) =  0.5d0 - t
        sw(2,2) =  0.5d0 + t
        sw(2,3) =  sw(2,2)
        sw(2,4) =  sw(2,1)

      elseif(l.eq.5) then

        t       =  sqrt(1120.0d0)

        sw(1,1) = (70.d0+t)/126.d0
        sw(1,2) = (70.d0-t)/126.d0

        t       =  1.d0/(15.d0 * (sw(1,2) - sw(1,1)))

        sw(2,1) = (5.0d0*sw(1,2) - 3.0d0)*t/sw(1,1)
        sw(2,2) = (3.0d0 - 5.0d0*sw(1,1))*t/sw(1,2)
        sw(2,3) =  2.0d0*(1.d0 - sw(2,1) - sw(2,2))
        sw(2,4) =  sw(2,2)
        sw(2,5) =  sw(2,1)

        sw(1,1) = -sqrt(sw(1,1))
        sw(1,2) = -sqrt(sw(1,2))
        sw(1,3) =  0.0d0
        sw(1,4) = -sw(1,2)
        sw(1,5) = -sw(1,1)

!     Compute points and weights

      else

        call gausspw(l,sw)

      endif

      end subroutine int1dg

      subroutine gausspw (nn,sw)

!-----[--.----+-!--.----+----.----+------------------------------------]
!     Input:
!     nn       = Number of Gauss Points to compute

!     Outputs:
!     sw(1,*)  = Gauss Point Coordinates
!     sw(2,*)  = Gauss Point Weights
!-----[--.----+-!--.----+----.----+------------------------------------]
      implicit   none

      integer       :: nn, n
      real (kind=8) :: sw(2,*)
      real (kind=8) :: fn, beta, cc, xt, dpn,pn1, flgama

      fn   = dble(nn)
      beta = exp(2.d0*flgama(1.d0) - flgama(2.d0))
      cc   = 2.d0*beta
      do n = 2,nn
        cc = cc*4.d0*dble(n-1)**4
     &       / (dble(2*n-1)*dble(2*n-3)*dble(2*n-2)**2)
      end do ! n

      do n = 1,nn

!       Largest zero

        if(n.eq.1) then
          xt = 1.d0 - 2.78d0/(4.d0 + fn*fn)

!       Second zero

        elseif(n.eq.2) then
          xt = xt - (4.1d0 + 0.246d0*(fn - 8.d0)/fn)*(1.d0 - xt)

!       Third zero

        elseif(n.eq.3) then
          xt = xt - (1.67d0 + 0.3674d0*(fn - 8.d0)/fn)*(sw(1,1) - xt)

!       Second last zero

        elseif(n.eq.nn-1) then
          xt = xt + (xt - sw(1,n-2))/0.766d0/(1.d0 + 0.639d0*(fn-4.d0)
     &                                      /(1.d0 + 0.710d0*(fn-4.d0)))

!       Last zero

        elseif(n.eq.nn) then
          xt = xt + (xt - sw(1,n-2))/1.67d0/(1.d0 + 0.22d0*(fn-8.d0)/fn)

!       Intermediate roots

        else
          xt    = 3.d0*sw(1,n-1) - 3.d0*sw(1,n-2) + sw(1,n-3)
        endif

!       Find root using xt-value

        call root (xt,nn,dpn,pn1)
        sw(1,n) = xt
        sw(2,n) = cc/(dpn*pn1)

      end do ! n

!     Reverse order of points

      do n = 1, nn
        sw(1,n) = - sw(1,n)
      end do ! n

      end subroutine gausspw

      subroutine root (x,nn,dpn,pn1)

!-----[--.----+-!--.----+----.----+------------------------------------]
!      Improve approximate root x; in addition we also obtain
!         dpn = derivative of p(n) at x
!         pn1 = value of p(n-1) at x
!-----[--.----+-!--.----+----.----+------------------------------------]
      implicit   none

      logical       :: notconv
      integer       :: nn, iter
      real (kind=8) :: x,dpn,pn1, d,p,dp

      real (kind=8) ::  eps
      data       eps / 1.d-39 /

      iter = 0

      notconv = .true.
      do while(notconv .and. iter.lt.50)
        iter = iter + 1
        call recur (p,dp,pn1,x,nn)
        d = p/dp
        x = x - d
        if(abs(d).le.eps) then
          notconv = .false.
        endif
      end do ! while
      dpn = dp

      end subroutine root

      subroutine recur (pn,dpn,pn1,x,nn)

      implicit   none

      integer       :: nn, n
      real (kind=8) :: pn,dpn,pn1,x, c
      real (kind=8) :: p1, p,dp, dp1, q,dq

      p1  = 1.d0
      p   = x
      dp1 = 0.d0
      dp  = 1.d0
      do n = 2,nn
        c   = 4.d0*dble(n-1)**4
     &       / (dble(2*n-1)*dble(2*n-3)*dble(2*n-2)**2)
        q   = x*p  - c*p1
        dq  = x*dp - c*dp1 + p
        p1  = p
        p   = q
        dp1 = dp
        dp  = dq
      end do ! n
      pn  = p
      dpn = dp
      pn1 = p1

      end subroutine recur

      function flgama (w)

      implicit none

      integer       :: m, i
      real (kind=8) :: flgama, w, pi,x, p, fk, y, z,zz

      pi =  acos(-1.d0)
      x  =  w
      fk = -1.d0

!     w less eq 0.5

      if (x .lt. 0.5d0) then
        m = 1
        p = pi/sin(x*pi)
        x = 1.d0 - x
      else
        m = 0
        p = 0.0d0
      endif

      do while(x + fk - 6.d0 .le.0.0d0)
        fk = fk + 1.d0
      end do ! while

      z  = x + fk
      zz = z*z

      y  = (z - 0.5d0)*log(z) - z + 0.9189385332047d0 + (((((-4146.d0/zz
     &        + 1820.d0)/zz - 1287.d0)/zz + 1716.d0)/zz - 6006d0)/zz
     &        + 180180.d0)/z/2162160.d0

      if(fk.gt.0.0d0) then
        do i = 1,int(fk)
          fk = fk - 1.d0
          y  = y - log(x + fk)
        end do ! i
      endif

      if(m.ne.0) then
        if(p.le.0.0d0) then
          write (*,2000) w
          y = 0.d0
        else
          y = log(p) - y
        endif
      endif
      flgama = y

2000  format (2x,'gamma(',e11.4,') is negative')

      end function flgama
