!$Id:$
      subroutine shpi2d(sg,xsj,xl,ndm)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Enhanced mode shaped functions

!      Inputs:

!      Outputs:
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'incshp.h'

      integer       :: ndm
      real (kind=8) :: xsj, sg(2),xl(ndm,*)

      save

!     Compute enhanced strain 'incompatible' shape functions

      shpi(1,1) = -sg(1)*(-xl(2,1) - xl(2,2) + xl(2,3) + xl(2,4))/xsj
      shpi(2,1) =  sg(1)*(-xl(1,1) - xl(1,2) + xl(1,3) + xl(1,4))/xsj
      shpi(3,1) =  0.0d0
      shpi(1,2) =  sg(2)*(-xl(2,1) + xl(2,2) + xl(2,3) - xl(2,4))/xsj
      shpi(2,2) = -sg(2)*(-xl(1,1) + xl(1,2) + xl(1,3) - xl(1,4))/xsj
      shpi(3,2) =  0.0d0
      shpi(1,3) =  0.0d0
      shpi(2,3) =  0.0d0
      shpi(3,3) =  sg(1)*sg(2)

      end subroutine shpi2d
