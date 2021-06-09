!$Id:$
      subroutine pdefm(x,b,c,angl,ndm,ndf,numnp, dr, flag)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute deformed position of nodes

!      Inputs:
!         x(ndm,*)  - Nodal coordinates of mesh
!         b(ndf,*)  - Solution vector to add to coordinates
!         c         - Scale factor for added solution
!         angl(*)   - Value of boundary angle for node
!         ndm       - Dimension of x array
!         ndf       - Number dof/node
!         numnp     - Number of nodes in mesh
!         flag      - Check angle if true

!      Outputs:
!         dr(ndf,*) - Deformed coordinates
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'pointer.h'
      include  'comblk.h'

      include  'pdata4.h'

      logical       :: flag
      integer       :: ndm,ndf,numnp, i,n
      real (kind=8) :: c, cn,sn
      real (kind=8) :: x(ndm,*),b(ndf,*),angl(*),uu(3), vv(15), dr(3,*)

      save

      do n = 1,numnp
        if(mr(npty-1+n).ge.0) then
          do i = 1,ndf
            vv(i) = b(i,n)
          end do ! i
          if(flag) then
            if(ndm.gt.1 .and. ndf.gt.1 .and. angl(n).ne.0.0d0) then
              call pdegree(angl(n), sn,cn)
              vv(1) = b(1,n)*cn - b(2,n)*sn
              vv(2) = b(1,n)*sn + b(2,n)*cn
            endif
          endif
          do i = 1,3
            if(pdf(i).gt.0 .and. pdf(i).le.ndf) then
              uu(i) = vv(pdf(i))
            else
              uu(i) = 0.0d0
            endif
          end do ! i
          do i = 1,ndm
            dr(i,n) = x(i,n) + c*uu(i)
          end do ! i
          do i = ndm+1,3
            dr(i,n) = c*uu(i)
          end do ! i
        endif
      end do ! n

      end subroutine pdefm
