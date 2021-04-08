!$Id:$
      subroutine pangl(ix,nen,angl,angg,nrot)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set value of angle for each element node

!      Inputs:
!         ix(nen) - Element nodal connection list
!         nen     - Number of nodes connected to element
!         angg(*) - Nodal angle array

!      Outputs:
!         angl(*) - Element angle array
!         nrot    - Number of nodes needing modification
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: nen,nrot,ii,n
      integer       :: ix(nen)
      real (kind=8) :: angl(nen),angg(*)

      save

!     Set up table of rotation angles

      nrot = 0
      do n = 1,nen
         angl(n) = 0.0d0
         ii = ix(n)
         if (ii.gt.0) then
            if (angg(ii).ne.0.0d0) then
               angl(n) = angg(ii)
               nrot = nrot + 1
            endif
         endif
      end do

      end subroutine pangl
