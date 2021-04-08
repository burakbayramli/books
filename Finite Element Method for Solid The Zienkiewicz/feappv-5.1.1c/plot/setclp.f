!$Id:$
      subroutine setclp(x,ndm,numnp)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute clip range from mesh data

!      Inputs:
!         x(ndm,*)  - Nodal coordinates of mesh
!         ndm       - Spatial dimension of region
!         numnp     - Number of nodes in mesh

!      Outputs:
!         none      - Output through common /plclip/
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'plclip.h'
      include  'pointer.h'
      include  'comblk.h'

      logical       :: flag
      integer       :: i,n,ndm,numnp
      real (kind=8) :: x(ndm,*)

      save

      flag = .true.
      do n = 1,numnp
        if(mr(npty-1+n).ge.0) then

!         Set defaults to an existing coordinate

          if( flag ) then
            do i = 1,ndm
              cmin(i) = x(i,n)
              cmax(i) = x(i,n)
            end do
            flag = .false.
          end if

!         Set min / max for other coordinates

          do i = 1,ndm
            cmin(i) = min( cmin(i), x(i,n))
            cmax(i) = max( cmax(i), x(i,n))
          end do
        end if
      end do

      end subroutine setclp
