!$Id:$
      subroutine maxcor(x,ndm,numnp)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Determine max/min coordinates for a perspective
!               view selection

!      Inputs:
!         x(ndm,*)  - Nodal coordinates (may be in deformed state)
!         ndm       - Spatial dimension
!         numnp     - Number of nodes in mesh

!      Outputs:
!         none      - Output through common /pdata0/
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'pdata0.h'
      include  'pdatas.h'
      include  'pointer.h'
      include  'comblk.h'

      integer       :: ndm, numnp, i,ii,n
      real (kind=8) :: x(ndm,*)

      save

!     Determine max/min coordinates for a perspective view selection

      ii  = min(ndm,3)
      do i = 1,ii
        vmin(i) = x(i,1)
        do n = 1,numnp
            vmin(i) = max(vmin(i),x(i,n))
        end do
        vmax(i) = vmin(i)
        do n = 1,numnp
          if(mr(npty-1+n).ge.0) then
            vmin(i) = min(vmin(i),x(i,n))
          endif
        end do
      end do

      end subroutine maxcor
