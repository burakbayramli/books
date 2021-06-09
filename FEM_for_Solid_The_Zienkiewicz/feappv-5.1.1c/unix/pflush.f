!$Id:$
      subroutine pflush(ifile)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    25/05/2009
!       1. Delete call to flush()                           29/12/2014
!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Flush file buggers

!     Input:
!        ifile   - Logical unit number

!     Output:
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      integer        :: ifile
      logical        :: opnfil
      integer        :: u

!     Flush buffer on units

      if(ifile.gt.0) then
        call flush(ifile)
      else
        do u = 7,99
          inquire(unit=u, opened= opnfil)
          if(u.ne.15 .and. opnfil) then
            call flush(u)
          endif
        end do
      endif

      end subroutine pflush
