!$Id:$
      subroutine pexpd(a,t,id,neq,nneq)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Expand compressed vector to include all values
!               removed by boundary conditions.

!      Inputs:
!         a(*)      - Compressed vector
!         id(*)     - Equation number array
!         neq       - Number equations in compressed vector
!         nneq      - Number in uncompressed vector

!      Scratch:
!         t(*)      - Vector of length neq

!      Outputs:
!         a(*)      - Uncompressed vector
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: n,neq,nneq
      integer       :: id(*)
      real (kind=8) :: a(*),t(*)

!     Copy compressed to temporary vector

      do n = 1,neq
        t(n) = a(n)
      end do

!     Exand compressed temporary vector

      do n = 1,nneq
        if(id(n).gt.0) then
          a(n) = t(id(n))
        else
          a(n) = 0.0d0
        endif
      end do

      end subroutine pexpd
