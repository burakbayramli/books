c$Id:$
      subroutine reshis(ix,nen1,numel,n1, n2)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Initialize t_n+1 history variables from final value
c               of variables at t_n

c      Inputs:
c         ix(nen1,*)  - Element connection/history pointer array
c         nen1        - Dimension of ix array
c         numel       - Number of elements in mesh
c         n1          - Pointer in ix to t_n   data
c         n2          - Pointer in ix to t_n+1 data

c      Outputs:
c         none        - Output is retained in blank common
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'pointer.h'
      include  'comblk.h'

      integer   nen1,numel,n1,n2, nh1,nh2, n,nh,nhd
      integer   ix(nen1,numel)

      save

c     Move history variables from location 'n1' to 'n2'

      do n = 1,numel
        nh1 = np(50) + ix(n1,n) - 1
        nh2 = np(50) + ix(n2,n) - 1
        if(nh2.ne.nh1) then
          nhd = abs(nh2 - nh1)
          do nh = 1,nhd
            hr(nh+nh2) = hr(nh+nh1)
          end do
        endif
      end do

      end
