c$Id:$
      subroutine protv(nty,u,angl,ndm,ndf,numnp, du)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Compute displacements in global Cartesian frame.

c      Inputs:
c         nty(*)    - Nodal type
c         u(ndf,*)  - Solution vector at nodes
c         angl(*)   - Value of angle for sloping b.c.
c         ndm       - Spatial dimension of mesh
c         ndf       - Number dof/node
c         numnp     - Number of nodes in mesh

c      Outputs:
c         du(ndf,*) - Cartesian displacements at nodes
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer   ndm,ndf,numnp, i,n, nty(*)
      real*8    ang,cn,sn
      real*8    u(ndf,*),angl(*),du(ndf,*)

      save

      do n = 1,numnp
        if(nty(n).ge.0) then
          do i = 1,ndf
            du(i,n) = u(i,n)
          end do
          if(ndm.gt.1 .and. ndf.gt.1 .and. angl(n).ne.0.0d0) then
            ang = angl(n)*0.017453292d0
            cn  = cos(ang)
            sn  = sin(ang)
            du(1,n) = u(1,n)*cn - u(2,n)*sn
            du(2,n) = u(1,n)*sn + u(2,n)*cn
          endif
        else
          do i = 1,ndf
            du(i,n) = 0.0d0
          end do
        endif
      end do

      end
