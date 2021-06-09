!$Id:$
      real function etime(tt)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Timing information for output file

!      Inputs:
!         none

!      Outputs:
!         tt(*)   - CPU and System time (no system time for DOS)
!         etime   - Total CPU + System time
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'etime1.h'

      real    (kind=4) :: tt(2)
      integer (kind=2) :: ihr, imin, isec, ihth

      save

      call gettim (ihr,imin,isec,ihth)
      tt(2) = 0.0
      tt(1) = 0.01*ihth + isec + 60.*(imin + 60.*ihr) - tim0
      if(tt(1) .lt. 0.0) then
        tt(1) = tt(1) + 86400.0
        tim0  = tim0  - 86400.0
      endif
      etime = tt(1) + tt(2)

      end function etime
