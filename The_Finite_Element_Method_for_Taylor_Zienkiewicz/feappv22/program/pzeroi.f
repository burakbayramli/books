c$Id:$
      subroutine pzeroi(ii,nn)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Zero integer array of data

c      Inputs:
c         nn     - Length of array

c      Outputs:
c         ii(*)  - Array with zero values
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer   n,nn
      integer   ii(nn)

      save

c     Zero integer array

      do n = 1,nn
        ii(n) = 0
      end do

      end
