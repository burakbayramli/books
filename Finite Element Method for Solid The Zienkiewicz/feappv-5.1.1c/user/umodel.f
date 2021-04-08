!$Id:$
      subroutine umodel(umat,eps,theta,td,d,ud,hn,h1,nh,ii,istrt,
     &                  sig,dd, isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: User Constitutive Model

!     Input:
!          umat    -  User material type
!          eps(*)  -  Current strains at point      (small deformation)
!                  -  Deformation gradient at point (finite deformation)
!          theta   -  Trace of strain at point
!                  -  Determinant of deforamtion gradient
!          td      -  Temperature change
!          d(*)    -  Program material parameters (ndd)
!          ud(*)   -  User material parameters (nud)
!          hn(nh)  -  History terms at point: t_n
!          h1(nh)  -  History terms at point: t_n+1
!          nh      -  Number of history terms
!          ii      -  Current point number
!          istrt   -  Start state: 0 = elastic; 1 = last solution
!          isw     -  Solution option from element

!     Output:
!          sig(6)  -  Stresses at point.
!          dd(6,6) -  Current material tangent moduli

!-----[--.----+----.----+----.-----------------------------------------]
      implicit none

      include 'iofile.h'

      integer       :: umat,nh,istrt,isw, ii
      real (kind=8) :: td
      real (kind=8) :: eps(*),theta(*),d(*),ud(*),hn(*),h1(*)
      real (kind=8) :: sig(*),dd(*)

      save

!     Material Model 1
      if(    umat.eq.1) then
        call umatl1(eps,theta,td,d,ud,hn,h1,nh,ii,istrt, sig,dd, isw)

!     Material Model 2
      elseif(umat.eq.2) then
        call umatl2(eps,theta,td,d,ud,hn,h1,nh,ii,istrt, sig,dd, isw)

!     Material Model 3
      elseif(umat.eq.3) then
        call umatl3(eps,theta,td,d,ud,hn,h1,nh,ii,istrt, sig,dd, isw)

!     Material Model 4
      elseif(umat.eq.4) then
        call umatl4(eps,theta,td,d,ud,hn,h1,nh,ii,istrt, sig,dd, isw)

!     Material Model 5
      elseif(umat.eq.5) then
        call umatl5(eps,theta,td,d,ud,hn,h1,nh,ii,istrt, sig,dd, isw)

!     Error no umat set
      else

        write(iow,4000)
        call plstop(.true.)

      endif

!     Format

4000  format(/' *ERROR* User model name incorrectly set.')

      end subroutine umodel
