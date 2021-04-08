!$Id:$
      subroutine pdetf(g,fdet)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/05/2018
!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Compute deformation gradient determinant from displacement
!              gradient
!     Input:
!       g(:,:)    - Displacement gradient
!     Output:
!       fdet      - Deformation gradient of I + g(:,:)
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      real (kind=8) :: g(3,3),fdet

      fdet = (1.d0+g(1,1))*((1.d0+g(2,2))*(1.d0+g(3,3)) - g(2,3)*g(3,2))
     &     + g(1,2)*(g(2,3)*g(3,1) - g(2,1)*(1.d0+g(3,3)))
     &     + g(1,3)*(g(2,1)*g(3,2) - (1.d0+g(2,2))*g(3,1))

      end subroutine pdetf
