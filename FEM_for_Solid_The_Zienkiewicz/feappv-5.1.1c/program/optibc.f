!$Id:$
      subroutine optibc(id,nnid,ndf,numnp)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Count active dof at each node

!     Inputs:
!       ndf        - Number dof at each node
!       numnp      - Number nodes
!       id(ndf,*)  - Equation numbers for nodes

!     Outputs:
!       nnid(*)    - Number active dof/node
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      integer        :: ndf,numnp, i,n
      integer        :: id(ndf,numnp),nnid(numnp)

      save

      do n = 1,numnp
        nnid(n) = 1
        nnid(n) = 0
        do i = 1,ndf
          if(id(i,n).gt.0) then
            nnid(n) = nnid(n) + 1
          endif
        end do ! i
      end do ! n

      end subroutine optibc
