!$Id:$
      subroutine strn2d(d,xl,ul,tl,shp,ndf,ndm,nel,xx,yy,ta,eps)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:

!      Inputs:

!      Outputs:
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'cdata.h'
      include  'incshp.h'
      include  'pmod2d.h'

      integer       :: ndf,ndm,nel, j
      real (kind=8) :: xx,yy,ta
      real (kind=8) :: d(*),xl(ndm,*),ul(ndf,nen,*),tl(*),shp(3,*)
      real (kind=8) :: eps(9,3)

      save

!     Compute strains and coordinates

      do j = 1,6
        eps(j,1) = 0.0d0
        eps(j,3) = 0.0d0
      end do
      xx = 0.0d0
      yy = 0.0d0
      ta = -d(9)
      do j = 1,nel
        xx = xx + shp(3,j)*xl(1,j)
        yy = yy + shp(3,j)*xl(2,j)
        ta = ta + shp(3,j)*tl(j)
        eps(1,1) = eps(1,1) + shp(1,j)*ul(1,j,1)
        eps(2,1) = eps(2,1) + shp(2,j)*ul(2,j,1)
        eps(3,1) = eps(3,1) + shp(3,j)*ul(1,j,1)
        eps(4,1) = eps(4,1) + shp(2,j)*ul(1,j,1)
     &                      + shp(1,j)*ul(2,j,1)
        eps(1,3) = eps(1,1) + shp(1,j)*ul(1,j,2)
        eps(2,3) = eps(2,1) + shp(2,j)*ul(2,j,2)
        eps(3,3) = eps(3,1) + shp(3,j)*ul(1,j,2)
        eps(4,3) = eps(4,1) + shp(2,j)*ul(1,j,2) + shp(1,j)*ul(2,j,2)
      end do

!     Compute enhanced strains

      if(etype.eq.3) then
        do j = 1,2
          eps(1,1) = eps(1,1) + shpi(1,j)*ui(2*j-1,1)
          eps(2,1) = eps(2,1) + shpi(2,j)*ui(2*j,1)
          eps(4,1) = eps(4,1) + shpi(1,j)*ui(2*j,1)
     &                        + shpi(2,j)*ui(2*j-1,1)
          eps(1,3) = eps(1,3) + shpi(1,j)*ui(2*j-1,2)
          eps(2,3) = eps(2,3) + shpi(2,j)*ui(2*j,2)
          eps(4,3) = eps(4,3) + shpi(1,j)*ui(2*j,2)
     &                        + shpi(2,j)*ui(2*j-1,2)
        end do
      endif

!     Strain at t_n

      eps(1,2) = eps(1,1) - eps(1,3)
      eps(2,2) = eps(2,1) - eps(2,3)
      eps(3,2) = eps(3,1) - eps(3,3)
      eps(4,2) = eps(4,1) - eps(4,3)

!     Set 3-strain (thickness/hoop)

      if(stype.eq.3) then
        eps(3,1) = eps(3,1)/xx
        eps(3,2) = eps(3,2)/xx
        eps(3,3) = eps(3,3)/xx
      else
        eps(3,1) = 0.0d0
        eps(3,2) = 0.0d0
        eps(3,3) = 0.0d0
      endif

      end subroutine strn2d
