!$Id:$
      subroutine phelp(cc,wd,ed,nwd,wrd)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Output help information for available commands

!      Inputs:
!         cc      - Command name to find
!         wd(*)   - List of commands
!         ed(*)   - Level of commands
!         nwd     - Number of commands
!         wrd     - Command for search

!      Outputs:
!         None    - Outputs to screen command information
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'hlpdat.h'

      character (len=4) :: wd(nwd),cc,sd(100),temp
      character         :: wrd*(*)

      logical       :: pcomp, prtman
      integer       :: i,j,ii,nwd,nsd
      integer       :: ed(nwd)

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

!     Help file for macro command list

      if(prtman) then

        nsd = 0
        do i = 1,nwd
          if(ed(i).le.hlplev) then
            nsd     = nsd + 1
            sd(nsd) = wd(i)
          end if
        end do

!       Sort to increasing order

        do i = 1,nsd-1
          ii = i
          do j = i+1,nsd
            if( sd(j).lt.sd(ii) ) ii = j
          end do

!         Swap entries

          if(ii.ne.i) then
            temp   = sd(i)
            sd(i)  = sd(ii)
            sd(ii) = temp
          endif
        end do

!       Output sorted list

        if(pcomp(wrd,'macr',4)) then
          write(*,2001)
          write(*,2000) (sd(i),i=1,nsd)
        else
          write(*,2003) wrd
          write(*,2000) (sd(i),i=1,nsd)
        end if

      end if

!     Formats

2000  format(11(3x,a4))

2001  format(/3x,'The following execution commands are available:'//
     &        3x,'Use of LOOP must terminate with a matching NEXT.'/
     &        3x,'Multiple LOOP-NEXT pairs may occur to depth of 8.'//
     &        3x,'Terminate with a QUIT (q) or END (e) command.'/)

2003  format(/3x,'The following ',a,' commands are available:'/
     &        3x,'Terminate execution with an END (e) command.'/)

      end subroutine phelp
