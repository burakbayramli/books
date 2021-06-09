!$Id:$
      subroutine pmacr4(ctl,lct,j)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Command language instruction control:  Part 4.
!               Interface to user command routines

!      Inputs:
!         ctl(*)   - Command parameters for current command
!         lct      - Command option for current command
!         j        - Command number to execute in this routine

!      Outputs:
!         Depends on value of j
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'print.h'

      character (len=15) :: lct

      integer       :: j
      real (kind=8) :: ctl(3)

      save

!     'macn' command
!     [macn,xxxx,n1,n2,n3]

      call umaclib(j,lct,ctl)

      end subroutine pmacr4
