!$Id:$
      subroutine pmacr7 (j)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Command language instruction subprogram: Part 7.

!     Inputs:
!        j        - Number of command to execute

!     Outputs:
!        Depends on value of command j: None for serial version
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      integer       :: j

      save

!     Solution command instruction subprogram - part 7.

      if(j.eq.1) then

        write(*,2000) ' GRAPh '

      endif

!     Formats

2000  format('  *ERROR*',a,'command only available in Parallel FEAP',
     &       ' version')

      end subroutine pmacr7
