c$Id:$
      subroutine phelp(cc,wd,ed,nwd,wrd)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Output help information for available commands

c      Inputs:
c         cc      - Command name to find
c         wd(*)   - List of commands
c         ed(*)   - Level of commands
c         nwd     - Number of commands
c         wrd     - Command for search

c      Outputs:
c         None    - Outputs to screen command information
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'hlpdat.h'

      logical   pcomp, prtman
      integer   i,j,ii,nwd,nsd
      integer   ed(nwd)
      character wd(nwd)*4,cc*4,sd(100)*4,temp*4, wrd*(*)

      save

      if(pcomp(wrd,'mesh',4)) then
        ii = 1
      elseif(pcomp(wrd,'macr',4)) then
        ii = 2
      elseif(pcomp(wrd,'plot',4)) then
        ii = 3
      endif
      prtman = .true.
      do i = 1,nwd
        if(pcomp(cc,wd(i),4)) then
          call pman(wd(i),ii)
          prtman = .false.
        endif
      end do

c     Help file for macro command list

      if(prtman) then

        nsd = 0
        do i = 1,nwd
          if(ed(i).le.hlplev) then
            nsd     = nsd + 1
            sd(nsd) = wd(i)
          end if
        end do

c       Sort to increasing order

        do i = 1,nsd-1
          ii = i
          do j = i+1,nsd
            if( sd(j).lt.sd(ii) ) ii = j
          end do

c         Swap entries

          if(ii.ne.i) then
            temp   = sd(i)
            sd(i)  = sd(ii)
            sd(ii) = temp
          endif
        end do

c       Output sorted list

        if(pcomp(wrd,'macr',4)) then
          write(*,2001)
          write(*,2000) (sd(i),i=1,nsd)
        else
          write(*,2003) wrd
          write(*,2000) (sd(i),i=1,nsd)
        end if

      end if

c     Formats

2000  format(11(3x,a4))

2001  format(/3x,'The following execution commands are available:'//
     &        3x,'Use of LOOP must terminate with a matching NEXT.'/
     &        3x,'Multiple LOOP-NEXT pairs may occur to depth of 8.'//
     &        3x,'Terminate with a QUIT (q) or END (e) command.'/)

2003  format(/3x,'The following ',a,' commands are available:'/
     &        3x,'Terminate execution with an END (e) command.'/)

      end
