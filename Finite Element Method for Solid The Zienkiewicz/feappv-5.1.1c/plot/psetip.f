!$Id:$
      subroutine psetip(ip,numel)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set integer array to its position value for sorting
!               elements by z-sort for hidden surface plots

!      Inputs:
!         numel    - Number of elements in mesh

!      Outputs:
!         ip(*)    - Integer array with positions set
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: n, numel
      integer       :: ip(numel)

      save

      do n = 1,numel
        ip(n) = n
      end do

      end subroutine psetip
