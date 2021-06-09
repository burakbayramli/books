!$Id:$
      subroutine uasble(s,p,ld,ns,afl,bfl,b)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--+---------+---------+---------+---------+---------+---------+-]
!     Purpose: User to assemble current matrix/vector
!              User is responsible for creating the storage locations
!              for global arrays.

!     Inputs:
!       s(ns,ns)  - element matrix
!       p(ns)     - element vector
!       ld(ns)    - local/global active equation numbers
!       ns        - size of arrays
!       afl       - Assemble s(ns,ns) into global storage
!       bfl       - Assemble p(ns)    into global storage
!       b(*)      - RHS

!     Outputs:
!       b(*)      - RHS (assembled)
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit  none

      integer       :: ns
      logical       :: afl,bfl

      integer       :: ld(ns)
      real (kind=8) :: s(ns,ns),p(ns), b(*)

      end subroutine uasble
