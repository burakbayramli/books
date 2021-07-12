c$Id:$
      logical function cknon0(v, nn )

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Check that real vector has a non-zero component

c      Inputs:
c         v(*)   - Vector of real numbers
c         nn     - Length of vector

c      Outputs:
c         cknon0 - true of non-zero entries exist; else false.
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer   n,nn
      real*8    v(*)

      cknon0 = .false.
      do n = 1,nn
        if(v(n).ne.0.0d0) then
          cknon0 = .true.
          return
         endif
      end do

      end
