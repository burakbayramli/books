c$Id:$
      subroutine pexpd(a,t,id,neq,nneq)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Expand compressed vector to include all values
c               removed by boundary conditions.

c      Inputs:
c         a(*)      - Compressed vector
c         id(*)     - Equation number array
c         neq       - Number equations in compressed vector
c         nneq      - Number in uncompressed vector

c      Scratch:
c         t(*)      - Vector of length neq

c      Outputs:
c         a(*)      - Uncompressed vector
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer   n,neq,nneq
      integer   id(*)
      real*8    a(*),t(*)

c     Copy compressed to temporary vector

      do n = 1,neq
        t(n) = a(n)
      end do

c     Exand compressed temporary vector

      do n = 1,nneq
        if(id(n).gt.0) then
          a(n) = t(id(n))
        else
          a(n) = 0.0d0
        endif
      end do

      end
