!$Id:$
      subroutine piacel(ml,dr,a,neq)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute starting acceleration for transient problems
!               with diagonal (lumped) mass type arrays

!      Inputs:
!         ml(*)    - Diagonal mass type array
!         dr(*)    - Residual
!         neq      - Number of active equations

!      Outputs:
!         a(*)     - Initial acceleration
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: n,neq
      real (kind=8) :: ml(*),dr(*),a(*)

      save

!     Compute starting acceleration

      do n = 1,neq
        if(ml(n).ne.0.0d0) then
          a(n) = dr(n)/ml(n)
        else
          a(n) = 0.0d0
        endif
      end do

      end subroutine piacel
