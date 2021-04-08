!$Id:$
      subroutine setmac(yyy,wd,nwd,ll,jct,lct,lzz)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set macro commands for procedures use

!      Inputs:
!         yyy      - String with command name
!         wd(nwd)  - Active command names
!         nwd      - Number of active commands

!      Outputs:
!         ll       - Execution command numbers
!         jct(*)   - List of command numbers
!         lct(*)   - String for command option
!         lzz(*)   - String for command paramaters
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      logical       :: pcomp
      integer       :: ll, i,j, nwd

      character (len=80) :: yyy,lzz(*)
      character (len=47) :: temp
      character (len=45) :: xxx
      character (len=15) :: clab2,lct(*)
      character (len=4)  :: clab1,wd(nwd)

      integer       :: jct(*)

      save

      read(yyy,1000) clab1,clab2,xxx

!     Repack parameter string

      temp( 1:15) = xxx( 1:15)
      temp(16:16) = ','
      temp(17:31) = xxx(16:30)
      temp(32:32) = ','
      temp(33:47) = xxx(31:45)

!     Strip any blanck characters

      xxx = ' '
      j = 0
      do i = 1,47
        if(temp(i:i).ne.' ') then
          j = j + 1
          xxx(j:j) = temp(i:i)
        end if
      end do ! i

!     Insert data into command list

      do i = 1,nwd
        if(pcomp(clab1,wd(i),4)) then
          ll      = ll + 1
          jct(ll) = i
          lct(ll) = clab2
          lzz(ll) = xxx
          return
        end if
      end do
      write(*,3000) clab1

!     Formats

1000  format(a4,11x,a15,a)

3000  format(' *WARNING* Illegal command ',a4,' in procedure.')

      end subroutine setmac
