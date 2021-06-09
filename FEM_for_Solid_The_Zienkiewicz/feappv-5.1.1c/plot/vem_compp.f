!$Id:$
      subroutine vem_compp(k, iplt, nel, iu)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

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

      integer       :: k, nel, iu, iplt(*)

      write(*,*) ' Option available only for VEM version'

      end subroutine vem_compp
