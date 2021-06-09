!$Id:$
      subroutine umod1d(umat,eps,td,d,ud,hn,h1,nh,ii,istrt, sig,dd, isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: User Constitutive Model

!     Input:
!          umat    -  User material type
!          eps     -  Current strain at point
!          td      -  Temperature change
!          d(*)    -  System material parameters
!          ud(*)   -  User material parameters
!          hn(nh)  -  History terms at point: t_n
!          h1(nh)  -  History terms at point: t_n+1
!          nh      -  Number of history terms
!          ii      -  Number of calls to constitution/element
!          istrt   -  Start condition on iteration: 0 = elastic
!          isw     -  Element control parameter

!     Output:
!          sig     -  Stress at point.
!          dd      -  Current material tangent modulus

!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: umat,nh,ii,istrt,isw
      real (kind=8) :: td, eps,sig,dd

      real (kind=8) :: d(*),ud(*),hn(nh),h1(nh)

      save

!     Material Model 1
      if(umat.eq.1) then

!       Dummy elastic model:  sig = E*eps
        dd  = d(1)
        sig = d(1)*eps

      endif

      end subroutine umod1d
