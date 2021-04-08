!$Id:$
      subroutine modify(p,s,dul,nsiz,nst)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Modify element residual for effects of specified
!               boundary values.

!               p(i) = p(i) - s(i,j)*dul(j)

!      Inputs:
!         ld(*)  - Array with negative entries where boundary
!                  solution to be imposed
!         s(*,*) - Element tangent array
!         dul(*) - Value of specified solution increments
!         nst    - Dimension of element arrays

!      Outputs:
!         p(*)   - Residual modified for effect of increments
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: nsiz,nst,i,j
      real (kind=8) :: p(nst),s(nst,nst),dul(nst)

!     Loop over columns and search for boundary terms

      do j = 1,nsiz

!       Loop over rows to modify active equations

        do i = 1,nsiz
          p(i) = p(i) - s(i,j)*dul(j)
        end do ! i

      end do ! j

      end subroutine modify
