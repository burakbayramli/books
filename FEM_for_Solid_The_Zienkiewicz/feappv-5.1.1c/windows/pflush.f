!$Id:$
      subroutine pflush(ifile)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    25/05/2009
!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Flush file buggers

!     Input:
!        ifile   - Logical unit number

!     Output:
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      integer        :: ifile

!     Flush buffer on units: Dummy routine for windows

      end subroutine pflush
