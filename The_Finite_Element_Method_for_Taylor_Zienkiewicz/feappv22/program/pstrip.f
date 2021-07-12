c$Id:$
      subroutine pstrip(xxx,yyy,i)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Strip off comments (begin with !) & leading blanks.

c      Inputs:
c         yyy(*)    - Input string
c         i         - First character to look for ! on comments
c                     N.B. Command language uses !x and !! for
c                          re-execution of command.

c      Outputs:
c         xxx(*)    - Output string after strips
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'

      logical   cksep
      character xxx*255,yyy*255
      integer   i,n,nn

      save

c     Strip comments

      do n = i,255
        if(ichar(yyy(n:n)).eq.13) then
          yyy(n:n) = ' '
        elseif(ichar(yyy(n:n)).eq.9) then
          yyy(n:n) = ' '
        elseif(yyy(n:n).eq.'!') then
          yyy(n:255) = ' '
          go to 100
        end if
      end do

c     Strip leading blanks

100   xxx = ' '
      do n = 1,255
        if(yyy(n:n).ne.' ') then
          xxx(1:256-n) = yyy(n:255)
          go to 200
        end if
      end do
      return

c     Find last character

200   do nn = 255,1,-1
        if(xxx(nn:nn).ne.' ') go to 300
      end do
      nn = 2

c     Remove extra blanks

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

      end
