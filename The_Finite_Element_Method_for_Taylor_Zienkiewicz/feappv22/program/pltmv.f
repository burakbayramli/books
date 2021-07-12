c$Id:$
      subroutine pltmv(pl,ipl,u,nplts,save)


c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Extract information for time history from solution
c               array

c      Inputs:
c         ipl(*) - Location of information in u array
c         u(*)   - Array of values
c         nplts  - Number of items to extract
c         save   - Sign of quantity to save

c      Outputs:
c         pl(*)  - Array of time history information
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      real*8    save

      integer   nplts,j,n, ipl(2,nplts)
      real*8    pl(nplts),u(*)

      do n = 1,nplts
        j = ipl(1,n)
        if(j.gt.0) then
          pl(n) = u(j)*save
        end if
      end do

      end
