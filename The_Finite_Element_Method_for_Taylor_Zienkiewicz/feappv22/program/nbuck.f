c$Id:$
      function nbuck(x,xmd,xmn,ndm,nsize)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Determine bucket number for bucket searche routine

c      Inputs:
c         x(3)     - Coordinate to determine bucket number
c         xmd(3)   - Size of bucket
c         xmn      - Minimum coordinate of bucket
c         ndm      - Spatial dimension of mesh
c         nsize(4) - Number of buckets/direction: (4) indicates
c                    if all points to be placed in one bucket.

c      Outputs:
c         nbuck    - Bucket number containing data point x
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer   nbuck
      integer   ndm,nn,ni,i, nsize(4)
      real*8    x(3),xmd(3),xmn(3)

      if(nsize(4).gt.1) then
        nn = (x(1) - xmn(1))/xmd(1)
        nn = max(0,min(nsize(1)-1,nn))
        do i = 2,ndm
          ni = (x(i) - xmn(i))/xmd(i)
          nn = nsize(i)*nn + max(0,min(nsize(i)-1,ni))
        end do
        nbuck = nn + 1
      else
        nbuck = 1
      endif

      end
