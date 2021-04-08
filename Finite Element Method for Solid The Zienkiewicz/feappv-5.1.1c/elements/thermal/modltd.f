!$Id:$
      subroutine modltd(d, ta,tgrad,hn,h1,nh, dd,flux,rhoc, isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved
!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    19/11/2009
!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Thermal material model driver

!     Input parameters
!          d(*)      -  up to ndd-nud-1 material parameters
!          hn(*)     -  Values ta t_n
!          ta        -  Temperature
!          tgrad(3)  -  Temperature gradient

!          isw       -  FEAP switch value

!     Output parameters
!          h1(*)     -  Values ta t_n+1
!          dd(3,3)   -  Thermal conductivity tensor
!          flux(3)   -  Thermal flux
!          rhoc      -  Density times specific heat
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'oelmt.h'
      include   'setups.h'

      integer       :: isw, tmat, nh
      real (kind=8) :: ta, psi,cs,sn,c2,s2, rhoc
      real (kind=8) :: d(*), tgrad(3), dd(3,3), flux(3), hn(*),h1(*)

!     Set thermal material type number

      tmat = nint(d(193))

!     Multiscale material model

      if(tmat.eq.1) then

        call trvemat(d, ta,tgrad,hn,h1,nh, flux,dd,rhoc, isw)

!     Fourier linear model

      elseif(tmat.eq.2) then

!       compute conductivity tensor

        psi = d(31)
        cs  = cos(psi)
        sn  = sin(psi)
        c2  = cs*cs
        s2  = sn*sn
        cs  = cs*sn

        dd(1,1) = c2*d(61) + s2*d(62)
        dd(1,2) = cs*(d(61) - d(62))
        dd(1,3) = 0.0d0

        dd(2,1) = dd(1,2)
        dd(2,2) = s2*d(61) + c2*d(62)
        dd(2,3) = 0.0d0

        dd(3,1) = 0.0d0
        dd(3,2) = 0.0d0
        dd(3,3) = d(63)

!       Set flux

        flux(1) = -dd(1,1)*tgrad(1) - dd(1,2)*tgrad(2)
        flux(2) = -dd(2,1)*tgrad(1) - dd(2,2)*tgrad(2)
        flux(3) = -dd(3,3)*tgrad(3)

!       Density * specific heat

        rhoc = d(4)*d(64)

!     Error

      else

        write(*,*) ' No properties specified for thermal element'
        call plstop(.true.)

      endif

      end subroutine modltd
