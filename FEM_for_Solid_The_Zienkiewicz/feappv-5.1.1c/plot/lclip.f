!$Id:$
      logical function lclip( ix, nen, x, ndm )

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Clip plot at specified planes

!      Inputs:
!         ix(*)     - List of nodes on element
!         nen       - Number of nodes on element
!         x(ndm,*)  - Nodal coordinates of element
!         ndm       - Spatial dimension of mesh

!      Outputs:
!         lclip     - Flag, true if element is within clip region
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'plclip.h'

      integer       :: i, n, nen,ndm

      integer       :: ix(nen)
      real (kind=8) :: x(ndm,*),x0(3)

      save

      do i = 1, ndm
        x0(i) = 0.0d0
        do n = 1,nen
          if(ix(n).gt.0) x0(i) = x0(i) + x(i,ix(n))
        end do
        x0(i) = x0(i)/nen
      end do
      if(ndm.eq.1) then
        lclip = (x0(1).ge.cmin(1)) .and. (x0(1).le.cmax(1))
      elseif(ndm.eq.2) then
        lclip = (x0(1).ge.cmin(1)) .and. (x0(1).le.cmax(1))
     &                             .and.
     &          (x0(2).ge.cmin(2)) .and. (x0(2).le.cmax(2))
      elseif(ndm.eq.3) then
        lclip = (x0(1).ge.cmin(1)) .and. (x0(1).le.cmax(1))
     &                             .and.
     &          (x0(2).ge.cmin(2)) .and. (x0(2).le.cmax(2))
     &                             .and.
     &          (x0(3).ge.cmin(3)) .and. (x0(3).le.cmax(3))
      else
        lclip = .false.
      endif

      end function lclip
