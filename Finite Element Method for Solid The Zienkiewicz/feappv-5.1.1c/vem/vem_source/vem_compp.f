!$Id:$
      subroutine vem_compp(k, iplt, nel, iu)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Determine number of elements connected to each node
!               for use in mesh plot/outline routine xpline.

!      Inputs:
!         k           - VEM order
!         nel         - Number of nodes connected to element

!      Outputs:
!         iplt(*)     - Connection node list
!         iu          - Number of edge nodes
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      integer          :: k, nel, iu
      integer          :: iplt(*)

      integer          :: i, i1

      if(k.eq.1) then               ! Linear VEM
        do i = 1,nel
          iplt(i) = i
        end do ! i loop
        iu       = nel + 1
        iplt(iu) = 1
      elseif(k.eq.2) then           ! Quadratic VEM
        iu = nel/2
        i1 = 1
        do i = 1,iu
          iplt(i1  ) = i
          iplt(i1+1) = i + iu
          i1         = i1 + 2
        end do ! i loop
        iu       = 2*iu + 1
        iplt(iu) = 1
      endif

      end subroutine vem_compp
