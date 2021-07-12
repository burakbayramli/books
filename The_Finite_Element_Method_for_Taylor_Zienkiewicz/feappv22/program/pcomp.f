c$Id:$
      logical function pcomp(a,b,n)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Compare character strings for match
c               Ignores upper/lower case differences.

c      Inputs:
c         a(*)   - Character string 1
c         b(*)   - Character string 2
c         n      - Number of characters to compare

c      Outputs:
c         pcomp  - Flag, true if a = b
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer   n, inc, i, ia,ib
      character a*(*),b*(*)

      save

c     Logical function to determine match between alphanumeric data

      pcomp = .false.

c     Compute increment between an upper and lower case letter

      inc = ichar('A') - ichar('a')

c     Compare for match

      do i = 1,n

        ia = ichar(a(i:i))
        ib = ichar(b(i:i))

c       Test all permutations of characters for match

        if(ia.ne.ib .and. ia+inc.ne.ib .and. ia.ne.ib+inc ) return
      end do

      pcomp = .true.

      end
