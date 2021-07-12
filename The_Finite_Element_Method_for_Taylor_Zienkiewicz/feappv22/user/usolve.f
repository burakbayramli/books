c$Id:$
      subroutine usolve(flags, b)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c     Purpose:  User solver interface

c     Inputs:

c        flags(1) - Allocation and/or initialization phase
c        flags(2) - Perform factorization for direct solutions
c        flags(3) - Coefficient array unsymmetric
c        flags(4) - Solve equations
c        b(*)     - Vector for solution

c     Outputs:

c        flags(5) - True if error occurs
c        b(*)     - Vector of solution
c-----[--.----+----.----+----.-----------------------------------------]

      implicit none

      logical  flags(*)
      real*8   b(*)


      end
