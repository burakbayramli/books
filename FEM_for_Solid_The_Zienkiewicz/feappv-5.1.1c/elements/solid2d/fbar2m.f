!$Id:$
      subroutine fbar2m(f,xji,theta,lint)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Form F-bar and left Cauchy-Green tensors

!     Inputs:
!        f(3,3)      - Deformation gradient
!        xji(2,*)    - Determinant of deformation gradient (J)
!        theta(2,*)  - Mixed determinant of deformation gradient
!        lint        - Number of quadrature points

!     Outputs:
!        f(3,3)      - Mixed deformation gradient
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer       :: lint , i , l

      real (kind=8) :: ration, ratio1
      real (kind=8) :: f(9,2,*), xji(2,*),theta(2,*)

      save

!     Compute mixed deformation gradient

      do l = 1,lint
        ratio1 = (theta(1,l)/xji(1,l))**0.3333333333333333d0
        ration = (theta(2,l)/xji(2,l))**0.3333333333333333d0
        do i = 1,9
          f(i,1,l) = ratio1*f(i,1,l)
          f(i,2,l) = ration*f(i,2,l)
        end do
      end do

      end subroutine fbar2m
