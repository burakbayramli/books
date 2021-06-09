!$Id:$
      subroutine setext(ename,next, fext, flag)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:       Add extender file number

!      Inputs:
!        ename      - Name for extender
!        next       - Number of extender
!        flag       - Increment 'next' if .true.

!      Outputs:
!        fext(*)    - Extender with number 'next' added
!        next       - Next number
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'iofile.h'

      character      :: fext*(*), ename*(*)

      logical        :: flag
      integer        :: next

!     Set extender name and initial number

      fext(1:4) =  ename(1:4)
      fext(5:8) = '0000'
      if(next.le.9) then
        write(fext(8:8),'(i1)') next
      elseif(next.le.99) then
        write(fext(7:8),'(i2)') next
      elseif(next.le.999) then
        write(fext(6:8),'(i3)') next
      elseif(next.le.9999) then
        write(fext(5:8),'(i4)') next
      else
        write(iow,3000) ename
        call plstop(.true.)
      endif
      if(flag) next = next + 1

!     Format

3000  format(' *ERROR* PMESH: More than 10000 files for ',a)

      end subroutine setext
