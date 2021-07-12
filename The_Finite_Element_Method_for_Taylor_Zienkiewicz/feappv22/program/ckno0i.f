c$Id:$
      logical function ckno0i(iv, nn )

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Check that an integer vector has a non-zero component

c      Inputs:
c         iv(*)  - Vector of integers
c         nn     - Length of vector

c      Outputs:
c         ckno0i - true of non-zero entries exist; else false
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer   n,nn, iv(*)

      ckno0i = .false.
      do n = 1,nn
        if(iv(n).ne.0) then
          ckno0i = .true.
          return
         endif
      end do

      end
