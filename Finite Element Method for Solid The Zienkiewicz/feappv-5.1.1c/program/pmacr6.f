!$Id:$
      subroutine pmacr6 (lct,ct,j)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Command language instruction subprogram: Part 6

!     Inputs:
!        lct      - Command option for current command
!        ct(3)    - Command parameters for current command
!        j        - Number of command to execute

!     Outputs:
!        Depends on value of command j: None for serial version
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'codat.h'
      include   'print.h'

      character (len=15) :: lct

      integer       :: j
      logical       :: pcomp
      real (kind=8) :: ct(3)

      save

!     Solution command instruction subprogram - part 6.
!     [para] - input parameters for data use

      if(j.eq.1) then

        if(pcomp(lct,'    ',4)) then
          coflg = .true.
          call pconst(prt)
        else
          call setparam(lct,ct(1),prt)
        endif

      endif

      end subroutine pmacr6
