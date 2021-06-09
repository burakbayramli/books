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

      include   'debugs.h'

      integer            :: j
      character (len=15) :: text, lct
      real      (kind=8) :: ct(3)

      save

!     Solution command instruction subprogram - part 9.

      text = lct

!     [FE^2 <'start','get','send'>

      if(j.eq.1) then
        if(debug) write(*,2000) ' FE^2 ',lct
        call pfe2solv(lct)

      elseif(j.eq.2) then

        if(debug) write(*,2000) ' RVE    ',lct
        call pfe2rve(lct,ct)

      endif

!     Formats

2000  format(5x,'-->',a,' command for FE2 version: option ',a)

      end subroutine pmacr9
