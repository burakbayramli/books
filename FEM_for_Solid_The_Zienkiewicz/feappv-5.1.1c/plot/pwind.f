!$Id:$
      subroutine pwind(x,dr,ndm,ndf,numnp)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set plot window to accomodate both undeformed and
!               deformed view

!      Inputs:
!         x(ndm,*)  - Undeformed nodal coordinates
!         dr(ndf,*) - Deformed nodal coordinates
!         ndm       - Spatial dimension of mesh
!         ndf       - Number dof/node
!         numnp     - Number of nodes in mesh

!      Outputs:
!         dr(ndf,*) - Set to contain maximum range for each direction
!                     in node 1 and numnp positions
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: ndm,ndf,numnp, i,n

      real (kind=8) :: x(ndm,numnp),dr(ndf,numnp),xx(3,2)

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

      end subroutine pwind
