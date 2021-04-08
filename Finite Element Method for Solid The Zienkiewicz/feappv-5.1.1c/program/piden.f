!$Id:$
      subroutine piden(d,ns,ne)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Sets array to identity (1.0)

!      Inputs:
!         ns      - First entry to set
!         ne      - Last  entry to set

!      Outputs:

!         d(*)    - Array set to unity
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer       :: n,ns,ne
      real (kind=8) :: d(*)

      save

      do n = ns,ne
        d(n) = 1.0d0
      end do

      end subroutine piden
