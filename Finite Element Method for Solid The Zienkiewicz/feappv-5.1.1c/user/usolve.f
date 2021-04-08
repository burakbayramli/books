!$Id:$
      subroutine usolve(flags, b)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose:  User solver interface

!     Inputs:
!        flags(1) - Allocation and/or initialization phase
!        flags(2) - Perform factorization for direct solutions
!        flags(3) - Coefficient array unsymmetric
!        flags(4) - Solve equations
!        b(*)     - Vector for solution

!     Outputs:
!        flags(5) - True if error occurs
!        b(*)     - Vector of solution
!-----[--.----+----.----+----.-----------------------------------------]
      implicit none

      logical       :: flags(*)
      real (kind=8) :: b(*)

      end subroutine usolve
