c$Id:$
      subroutine chkblk(y,n0,nt)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Add zero to character array which has blank field.

c      Inputs:
c         y(*) - array to check
c         n0   - Field width of data
c         nt   - Size of array to check

c      Outputs:
c         y(*) - Blank fields have zero (0) added to field 'n0'

c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer   n0,nt,n
      character y*(*)

c     Add character if y(nt) is blank

      do n = n0,nt,n0
        if(y(n:n).eq.' ') y(n:n) = '0'
      end do

      end
