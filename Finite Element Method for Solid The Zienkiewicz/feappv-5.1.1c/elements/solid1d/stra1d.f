!$Id:$
      subroutine stra1d(d,xl,ul,tl,shp,ndf,ndm,nel,xx,ta,eps)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute strains at solution point

!      Inputs:
!        d(*)      : Material parameters
!        xl(ndm,*) : Nodal coordinates
!        ul(ndf,*) : Nodal solutions
!        tl(*)     : Nodal temperatures
!        shp(2,*)  : Shape functions
!        ndf       : Number nodal parameters
!        ndm       : Spatial dimension of mesh
!      Outputs:
!        xx        : Coordinate  at point
!        ta        : Temperature at point
!        eps(3,*)  : Strains at point
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cdata.h'
      include   'elcoor.h'
      include   'elpers.h'
      include   'incshp.h'
      include   'pmod2d.h'

      integer       :: ndf,ndm,nel, j
      real (kind=8) :: d(*),xl(ndm,*),ul(ndf,nen,*),tl(*),shp(2,*)
      real (kind=8) :: eps(9,3), xx,ta

      save

!     Compute strains and coordinates

      eps(:,1) = 0.0d0
      eps(:,3) = 0.0d0
      xx = 0.0d0
      ta = -d(9)
      do j = 1,nel
        xx = xx + shp(2,j)*xl(1,j)
        ta = ta + shp(2,j)*tl(j)
        eps(1,1) = eps(1,1) + shp(1,j)*ul(1,j,1)
        eps(3,1) = eps(3,1) + shp(2,j)*ul(1,j,1)
        eps(1,3) = eps(1,1) + shp(1,j)*ul(1,j,2)
        eps(3,3) = eps(3,1) + shp(2,j)*ul(1,j,2)
      end do ! j

!     Set reference and current coords

      xref(1) = xx
      xref(2) = 0.0d0
      xref(3) = 0.0d0
      xcur(1) = xx + eps(3,1)
      xcur(2) = 0.0d0
      xcur(3) = 0.0d0

!     Compute enhanced strains

      if(etype.eq.3) then
        eps(1,1) = eps(1,1) + shpi(1,1)*ui(1,1)
        eps(1,3) = eps(1,3) + shpi(1,1)*ui(1,2)
        if(stype.eq.3 .or. stype.eq.9) then
          eps(3,1)  = eps(3,1) + shpi(2,1)*ui(1,1)
          eps(3,3)  = eps(3,3) + shpi(2,1)*ui(1,2)
        endif
      endif

!     Strain at t_n

      eps(1,2) = eps(1,1) - eps(1,3)
      eps(3,2) = eps(3,1) - eps(3,3)

!     Set 3-strain (thickness/hoop) & Meridian strain (spherical)

      if(stype.eq.3) then             ! Axisymmetry
        eps(3,1:3) = eps(3,1:3)/xx
      elseif(stype.eq.9) then         ! Spherical symmetry
        eps(3,1:3) = eps(3,1:3)/xx
        eps(2,1:3) = eps(3,1:3)
      else                            ! Plane
        eps(3,1:3) = 0.0d0
      endif

      end subroutine stra1d
