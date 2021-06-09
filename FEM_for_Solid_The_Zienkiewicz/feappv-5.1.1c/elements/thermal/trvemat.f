!$Id:$
      subroutine trvemat(d, ta,tgrad, hn,h1,nh, flux,dd,rhoc, isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    19/11/2009
!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: User Constitutive Model for Thermal MPI3 Exchanges

!     Input:
!          ta      -  Temperature
!          hn(*)   -  Values at t_n
!          nh      -  Number values
!          tgrad(3)-  Thremal gradient
!          isw     -  Solution option from element

!     Output:
!          h1(*)   -  Values at t_n+1
!          d(*)    -  Material parameters
!          dd(3,3) -  Current material tangent moduli
!          rhoc    -  Density times specific heat
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      integer       :: nh, isw
      real (kind=8) :: ta, rhoc
      real (kind=8) :: d(*), tgrad(3), flux(3),dd(3,*), hn(*),h1(*)

!     Compute and output flux (q) and conductivities (moduli)

      save

!     DUMMY MODULE:  Multi-scale use only

      end
