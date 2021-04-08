!$Id:$
      subroutine histex(wd,clab1,jct,lct,ct,nwd,nlp,nnx,ll,is)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Control command language execution by history inputs

!      Inputs:
!         wd(*)    - List of solution commands
!         clab1    - Command to execute
!         nwd      - Number of commands in wd
!         nlp      - Loop command number
!         nnx      - Next command number
!         ll       - Command number to execute

!      Outputs:
!         jct(ll)  - Command number
!         lct(ll)  - Command option
!         lzz(ll)  - Variables for command
!         ct(3,ll) - Values evaluated from lzz
!         is       - Error if 0, otherwise 1
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'chdata.h'
      include  'comfil.h'
      include  'idata1.h'

      character (len=15) :: lct(*)
      character (len=4)  :: clab1,wd(nwd)
      character (len=1)  :: y

      logical       :: pcomp,errck,vinput,cinput
      integer       :: nwd,nlp,nnx,ll,i,is,n,nc

      integer       :: jct(*)
      real (kind=8) :: ct(3,*)

      save

      is = 0
      if(nn.le.0) then
        write(*,2000)
        is = 1
      else
        if(clab1(2:2).ne.'!') then
          nc = 3
          if(clab1(4:4).eq.' ') nc = 2
          if(clab1(3:3).eq.' ') nc = 1
          if(clab1(2:2).eq.' ') nc = 0
        else
          nc = 0
        endif
        if(nc.gt.0) then
          do n = nn,1,-1
            if(pcomp(clab1(2:2+nc),wd(js(n)),nc)) go to 102
          end do
          write(*,2001)
          is = 1
          return
        else
          n = nn
        endif

102    if(js(n).ne.nlp .and. js(n).ne.nnx) then
          jct(ll) = js(n)
          lct(ll) = ljs(n)
          lzz(ll) = lzs(n)
          errck   = vinput(lzz(ll),80,ct(1,ll),3)
!103       write(*,2002) wd(jct(ll)),lct(ll),(ct(i,ll),i=1,3)
          write(*,2002) wd(jct(ll)),lct(ll),(ct(i,ll),i=1,3)
!          read(*,1000,err=103,end=900) y
          if(.not.cinput( )) then
            goto 900
          end if
          y = record(1:1)
104       if(y.ne.' ' .and. y.ne.'y' .and. y.ne.'Y') is = 1
          return

!         Eof encountered

900       call  endclr ('PMACIO',y)
          goto 104
        else
          write(*,2003)
          is = 1
        endif
      endif

!     Formats

!1000  format(a1)

2000  format(/' *ERROR* No previous instruction of this type.')

2001  format(/' *ERROR* No match of this macro name')

2002  format(/' Macro to be executed.'/10x,'--> ',a4,1x,a15,1p,3e12.4
     &       /' Enter y or <CR> to accept.-->',$)

2003  format(/' *ERROR* loop/next execution not permitted')

      end subroutine histex
