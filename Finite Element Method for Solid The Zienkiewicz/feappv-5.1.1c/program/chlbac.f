!$Id:$
      subroutine chlbac(u,s,nn)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Back substitution for Cholesky factors in eigen
!               solutions.

!      Inputs:
!        u(*)   - Unreduced array
!        s(*,*) - Factored array of matrix
!        nn     - Size of arrays

!      Outputs:
!        u(*)   - Solution after back substitution

!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: i,j,jd, nn
      real (kind=8) :: u(*),s(nn,nn)

      save

!     Compute eigenvalues of general linear problem by backsubstitution

      j  = nn
      jd = nn*(nn+1)/2
      do i = 1,nn
        s(nn,i) = s(nn,i)*u(jd)
      end do

      do j = nn,2,-1
        jd = jd - j
        do i = 1,nn
          call colbac(u(jd+1),s(1,i),u(jd),j-1)
        end do
      end do

      end subroutine chlbac
