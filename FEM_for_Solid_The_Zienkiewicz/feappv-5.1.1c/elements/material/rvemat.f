!$Id:$
      subroutine rvemat(d,f,detf,ta,hn,hn1,nh, sig,dd,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    10/12/2007
!       1. Add allocation of 'RVEMA' and set to ma          13/04/2009
!       2. Add temperature to sends (was umatl2)            13/06/2009
!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: User Constitutive Model for MPI2 Exchanges

!     Input:
!          d(*)    -  Material parameters
!          f(3,3)  -  Deformation gradient at point (finite deformation)
!          detf    -  Determinant of deforamtion gradient
!          ta      -  Temperature change
!          hn(nh)  -  History terms at point: t_n
!          nh      -  Number of history terms
!          isw     -  Solution option from element

!     Output:

!          hn1(nh) -  History terms at point: t_n+1
!          sig(*)  -  Stresses at point.
!                     N.B. 1-d models use only sig(1)
!          dd(6,*) -  Current material tangent moduli
!                     N.B. 1-d models use only dd(1,1) and dd(2,1)
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      integer       :: nh,isw
      real (kind=8) :: ta
      real (kind=8) :: d(*),f(3,3),detf,hn(nh),hn1(nh), sig(*),dd(6,*)

!     Compute and output stress (sig) and (moduli)

      save

!     DUMMY MODULE:  Multi-scale use only from 'openmpi'

      end subroutine rvemat
