!$Id:$
      subroutine protv(nty,u,angl,ndm,ndf,numnp, du, flag)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute displacements in global Cartesian frame.

!      Inputs:
!         nty(*)    - Nodal type
!         u(ndf,*)  - Solution vector at nodes
!         angl(*)   - Value of angle for sloping b.c.
!         ndm       - Spatial dimension of mesh
!         ndf       - Number dof/node
!         numnp     - Number of nodes in mesh
!         flag      - Rotate if true

!      Outputs:
!         du(ndf,*) - Cartesian displacements at nodes
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      logical       :: flag
      integer       :: ndm,ndf,numnp, i,n, nty(*)
      real (kind=8) :: ang,cn,sn
      real (kind=8) :: u(ndf,*),angl(*),du(ndf,*)

      save

      do n = 1,numnp
        if(nty(n).ge.0) then
          do i = 1,ndf
            du(i,n) = u(i,n)
          end do
          if(flag) then
            if(ndm.gt.1 .and. ndf.gt.1 .and. angl(n).ne.0.0d0) then
              ang = angl(n)*0.017453292d0
              cn  = cos(ang)
              sn  = sin(ang)
              du(1,n) = u(1,n)*cn - u(2,n)*sn
              du(2,n) = u(1,n)*sn + u(2,n)*cn
            endif
          endif
        else
          do i = 1,ndf
            du(i,n) = 0.0d0
          end do
        endif
      end do ! n

      end subroutine protv
