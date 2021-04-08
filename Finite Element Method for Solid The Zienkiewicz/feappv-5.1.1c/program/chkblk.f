!$Id:$
      subroutine chkblk(y,n0,nt)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Add zero to character array which has blank field.

!      Inputs:
!         y(*) - array to check
!         n0   - Field width of data
!         nt   - Size of array to check

!      Outputs:
!         y(*) - Blank fields have zero (0) added to field 'n0'

!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      character    :: y*(*)
      integer      :: n0,nt,n

!     Add character if y(nt) is blank

      do n = n0,nt,n0
        if(y(n:n).eq.' ') y(n:n) = '0'
      end do

      end subroutine chkblk
