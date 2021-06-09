!$Id:$
      subroutine pltmv(pl,ipl,u,nplts,snsave)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Extract information for time history from solution
!               array

!      Inputs:
!         ipl(*) - Location of information in u array
!         u(*)   - Array of values
!         nplts  - Number of items to extract
!         snsave - Sign of quantity to save

!      Outputs:
!         pl(*)  - Array of time history information
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      real (kind=8) :: snsave

      integer       :: nplts,j,n, ipl(2,nplts)
      real (kind=8) :: pl(nplts),u(*)

      do n = 1,nplts
        j = ipl(1,n)
        if(j.gt.0) then
          pl(n) = u(j)*snsave
        end if
      end do

      end subroutine pltmv
