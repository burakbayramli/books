c$Id:$
      subroutine pmovec(id,a,b,nn)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose:  Move compressed array a into uncompressed array b

c      Inputs:
c         a(*)      - Compressed array to move
c         nn        - Length of uncompressed array

c      Outputs:
c         b(*)    - Uncompressed move of a (zero undefined values)
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer   n,nn,j
      real*8    a(nn),b(nn)
      integer   id(*)

      save

c     Move a-array into b-array

      do n = 1,nn
        j = id(n)
        if (j.gt.0) then
          b(n) = a(j)
        else
          b(n) = 0.0d0
        endif
      end do

      end
