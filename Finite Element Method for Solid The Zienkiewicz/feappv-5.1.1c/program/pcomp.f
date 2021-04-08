!$Id:$
      logical function pcomp(a,b,n)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compare character strings for match
!               Ignores upper/lower case differences.

!      Inputs:
!         a(*)   - Character string 1
!         b(*)   - Character string 2
!         n      - Number of characters to compare

!      Outputs:
!         pcomp  - Flag, true if a = b
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: n, inc, i, ia,ib
      character     :: a*(*),b*(*)

      save

!     Logical function to determine match between alphanumeric data

      pcomp = .false.

!     Compute increment between an upper and lower case letter

      inc = ichar('A') - ichar('a')

!     Compare for match

      do i = 1,n

        ia = ichar(a(i:i))
        ib = ichar(b(i:i))

!       Test all permutations of characters for match

        if(ia.ne.ib .and. ia+inc.ne.ib .and. ia.ne.ib+inc ) return
      end do

      pcomp = .true.

      end function pcomp
