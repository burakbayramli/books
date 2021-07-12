c$Id:$
      subroutine uasble(s,p,ld,ns,afl,bfl,b)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--+---------+---------+---------+---------+---------+---------+-]
c     Purpose: User to assemble current matrix/vector
c              User is responsible for creating the storage locations
c              for global arrays.

c     Inputs:
c       s(ns,ns)  - element matrix
c       p(ns)     - element vector
c       ld(ns)    - local/global active equation numbers
c       ns        - size of arrays
c       afl       - Assemble s(ns,ns) into global storage
c       bfl       - Assemble p(ns)    into global storage
c       b(*)      - RHS

c     Outputs:
c       b(*)      - RHS (assembled)
c-----[--+---------+---------+---------+---------+---------+---------+-]

      implicit  none

      integer   ns
      logical   afl,bfl

      integer   ld(ns)
      real*8    s(ns,ns),p(ns), b(*)

      end
