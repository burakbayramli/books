!$Id:$
      subroutine pmacr5 (lct,j)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Command language instruction subprogram: Part 5

!     Inputs:
!        lct      - Command option for current command
!        j        - Number of command to execute

!     Outputs:
!        Depends on value of command j: None for serial version
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'iofile.h'

      character (len=15) :: lct

      integer       :: j

      save

!     Solution command instruction subprogram - part 5.

      if(j.eq.1) then

        write(*,2000) ' OUTMesh '

!     [comm]ent,<message> Echo comment to screen when in batch mode

      elseif(j.eq.2) then
        if(ior.gt.0) write(*,2001) lct

      endif

!     Formats

2000  format('  *ERROR*',a,'command only available in FEAP version')
2001  format('   Comment: ',a)

      end subroutine pmacr5
