c$Id:$
      subroutine pwind(x,dr,ndm,ndf,numnp)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Set plot window to accomodate both undeformed and
c               deformed view

c      Inputs:
c         x(ndm,*)  - Undeformed nodal coordinates
c         dr(ndf,*) - Deformed nodal coordinates
c         ndm       - Spatial dimension of mesh
c         ndf       - Number dof/node
c         numnp     - Number of nodes in mesh

c      Outputs:
c         dr(ndf,*) - Set to contain maximum range for each direction
c                     in node 1 and numnp positions
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer   ndm,ndf,numnp, i,n

      real*8    x(ndm,numnp),dr(ndf,numnp),xx(3,2)

      save

      do i = 1,ndm
        xx(i,1) = x(i,1)
        xx(i,2) = x(i,1)
      end do

      do n = 1,numnp
        do i = 1,ndm
          xx(i,1) = min(xx(i,1),x(i,n),dr(i,n))
          xx(i,2) = max(xx(i,2),x(i,n),dr(i,n))
        end do
      end do

      do i = 1,ndm
        dr(i,1)     = xx(i,1)
        dr(i,numnp) = xx(i,2)
      end do

      end
