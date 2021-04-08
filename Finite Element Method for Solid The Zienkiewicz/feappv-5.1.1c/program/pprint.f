!$Id:$
      subroutine pprint(string)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Print prompt for interactive inputs

!      Inputs:
!         string - Character string to output

!      Outputs:
!         Writes prompt string to screen
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      character      :: string*(*)
      integer        :: l,le

      le = len(string)
      do l = le,1,-1
        if(string(l:l).ne.' ') exit
      end do ! l
      write(*,2000) string(1:l),' '

!     Format

2000  format(a,a,$)

      end subroutine pprint
