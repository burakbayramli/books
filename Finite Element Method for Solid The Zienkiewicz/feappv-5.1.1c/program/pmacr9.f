!$Id:$
      subroutine pmacr9 (lct,ct,j)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    25/02/2016
!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Command language instruction subprogram: Part 9 for FE^2

!     Inputs:
!        lct      - Command option for current command
!        ct(3)    - Command parameters for current command
!        j        - Number of command to execute

!     Outputs:
!        Depends on value of command j: None for serial version
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'comfil.h'
      include   'debugs.h'

      character (len=15) :: lct

      integer       :: j
      real (kind=8) :: ct(3)

      save

!     FE^2 Solution command instruction subprogram - part 9.

!     [FE^2

      if(j.eq.1) then

        write(*,2000) ' FE^2 ',lct,ct(:)

      elseif(j.eq.2) then

        write(*,2000) ' RVE    ',lct,ct(:)

      endif

!     Formats

2000  format('  *ERROR* ',a,' command only available in FE2 version:'/
     &     10x,'Option =',a,' CT(1:3) =',1p,3e12.4)

      end subroutine pmacr9
