!$Id:$
      subroutine shpi1d(sg,xl,ndm)

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
      real (kind=8) :: sg(2),xl(ndm,*)

!     Compute enhanced strain 'incompatible' shape functions

      shpi(1,1) = -4.0d0 * sg(1)/(xl(1,2) - xl(1,1))
      shpi(2,1) =  1.0d0 - sg(1)*sg(1)

      end subroutine shpi1d
