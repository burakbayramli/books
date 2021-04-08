!$Id:$
      subroutine pseqn(ip,numnp)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set sequential numbers to integer vector

!      Inputs:
!         numnp  - Number nodes in mesh

!      Outputs:
!         ip(*)  - Equation numbers
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: n, numnp
      integer       :: ip(*)

      save

      do n = 1,numnp
        ip(n) = n
      end do

      end subroutine pseqn
