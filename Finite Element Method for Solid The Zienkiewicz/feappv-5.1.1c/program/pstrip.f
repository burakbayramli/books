!$Id:$
      subroutine pstrip(xxx,yyy,i)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Strip off comments (begin with !) & leading blanks.

!      Inputs:
!         yyy(*)    - Input string
!         i         - First character to look for ! on comments
!                     N.B. Command language uses !x and !! for
!                          re-execution of command.

!      Outputs:
!         xxx(*)    - Output string after strips
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'

      character (len=256) :: xxx,yyy

      logical       :: cksep
      integer       :: i,n,nn

      save

!     Strip comments

      do n = i,256
        if(ichar(yyy(n:n)).eq.13) then
          yyy(n:n) = ' '
        elseif(ichar(yyy(n:n)).eq.9) then
          yyy(n:n) = ' '
        elseif(yyy(n:n).eq.'!') then
          yyy(n:256) = ' '
          go to 100
        end if
      end do

!     Strip leading blanks

100   xxx = ' '
      do n = 1,256
        if(yyy(n:n).ne.' ') then
          xxx(1:257-n) = yyy(n:256)
          go to 200
        end if
      end do
      return

!     Find last character

200   do nn = 256,1,-1
        if(xxx(nn:nn).ne.' ') go to 300
      end do
      nn = 2

!     Remove extra blanks

300   n = 1

301   if(xxx(n:n).eq.' ' .and. cksep(xxx(n+1:n+1))) then
        xxx(n:nn-1) = xxx(n+1:nn)
        xxx(nn:nn) = ' '
        nn = nn - 1
        go to 301
      endif
      n = n + 1
      if(n.lt.nn) go to 301

      do n = 1,nn-2
        if((xxx(n:n).eq.',' .or. xxx(n:n).eq.'=')
     &                     .and. xxx(n+1:n+1).eq.' ' ) then
          xxx(n+1:nn-1) = xxx(n+2:nn)
          xxx(nn:nn) = ' '
        endif
      end do

      end subroutine pstrip
