!$Id:$
      subroutine udebug(string,iopt)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/11/2006
!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose:  User solver interface

!     Inputs:
!       string      - Character string to output
!       iopt        - Integer

!     Outputs:
!       Write output to unit 'iunit'
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'setups.h'

      character (len=30) :: text
      character (len=8)  :: filnam
      character          :: string*(*)

      logical        :: lopen
      integer        :: iopt,iunit

!     Set unit number

      iunit = 90 + rank

!     Check for existing output file

      inquire(unit = iunit, opened = lopen)

      if(.not.lopen) then
        filnam = 'Udebug0'
        if(rank.le.10) then
          write(filnam(8:8),'(i1)') rank
        else
          write(filnam(7:8),'(i2)') rank
        endif
        open(unit = iunit, file = filnam)
      endif

      text = '  '
      text = string

      write(iunit,2000) text,iopt
      call pflush(iunit)

!     Output format

2000  format(a,'I =',i5)

      end subroutine udebug
