c$Id:$
      subroutine pdefm(x,b,c,angl,ndm,ndf,numnp, dr)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Compute deformed position of nodes

c      Inputs:
c         x(ndm,*)  - Nodal coordinates of mesh
c         b(ndf,*)  - Solution vector to add to coordinates
c         c         - Scale factor for added solution
c         angl(*)   - Value of boundary angle for node
c         ndm       - Dimension of x array
c         ndf       - Number dof/node
c         numnp     - Number of nodes in mesh

c      Outputs:
c         dr(ndf,*) - Deformed coordinates
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'pointer.h'
      include  'comblk.h'

      include  'pdata4.h'

      integer   ndm,ndf,numnp, i,n
      integer   nty
      real*8    c, cn,sn
      real*8    x(ndm,*),b(ndf,*),angl(*),uu(3), vv(15), dr(3,*)

      save

      nty = np(49) - 1
      do n = 1,numnp
        if(mr(nty+n).ge.0) then
          do i = 1,ndf
            vv(i) = b(i,n)
          end do
          if(ndm.gt.1 .and. ndf.gt.1 .and. angl(n).ne.0.0d0) then
            call pdegree(angl(n), sn,cn)
            vv(1) = b(1,n)*cn - b(2,n)*sn
            vv(2) = b(1,n)*sn + b(2,n)*cn
          endif
          do i = 1,3
            if(pdf(i).gt.0 .and. pdf(i).le.ndf) then
              uu(i) = vv(pdf(i))
            else
              uu(i) = 0.0d0
            endif
          end do
          do i = 1,ndm
            dr(i,n) = x(i,n) + c*uu(i)
          end do
          do i = ndm+1,3
            dr(i,n) = c*uu(i)
          end do
        endif
      end do

      end
