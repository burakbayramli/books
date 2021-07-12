c$Id:$
      subroutine pmove(a,b,nn)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Move real array a into b

c      Inputs:
c         a(*)      - Array to move
c         nn        - Length of array to move

c      Outputs:
c         b(*)      - Moved array
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer   n,nn
      real*8    a(nn),b(nn)

      save

c     Move a-array into b-array

      do n = 1,nn
        b(n) = a(n)
      end do

      end
