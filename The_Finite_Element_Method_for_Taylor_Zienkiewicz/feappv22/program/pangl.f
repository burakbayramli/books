c$Id:$
      subroutine pangl(ix,nen,angl,angg,nrot)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Set value of angle for each element node

c      Inputs:
c         ix(nen) - Element nodal connection list
c         nen     - Number of nodes connected to element
c         angg(*) - Nodal angle array

c      Outputs:
c         angl(*) - Element angle array
c         nrot    - Number of nodes needing modification
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer   nen,nrot,ii,n
      integer   ix(nen)
      real*8    angl(nen),angg(*)

      save

c     Set up table of rotation angles

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

      end
