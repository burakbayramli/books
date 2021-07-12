c$Id:$
      subroutine histex(wd,clab1,jct,lct,ct,nwd,nlp,nnx,ll,is)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Control command language execution by history inputs

c      Inputs:
c         wd(*)    - List of solution commands
c         clab1    - Command to execute
c         nwd      - Number of commands in wd
c         nlp      - Loop command number
c         nnx      - Next command number
c         ll       - Command number to execute

c      Outputs:
c         jct(ll)  - Command number
c         lct(ll)  - Command option
c         lzz(ll)  - Variables for command
c         ct(3,ll) - Values evaluated from lzz
c         is       - Error if 0, otherwise 1
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'chdata.h'
      include  'idata1.h'

      logical   pcomp,errck,vinput
      integer   nwd,nlp,nnx,ll,i,is,n,nc
      character clab1*4,lct(*)*15,wd(nwd)*4,y*1

      integer   jct(*)
      real*8    ct(3,*)

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
          errck   = vinput(lzz(ll),50,ct(1,ll),3)
103       write(*,2002) wd(jct(ll)),lct(ll),(ct(i,ll),i=1,3)
          read(*,1000,err=103,end=900) y
104       if(y.ne.' ' .and. y.ne.'y' .and. y.ne.'Y') is = 1
          return

c         Eof encountered

900       call  endclr ('PMACIO',y)
          goto 104
        else
          write(*,2003)
          is = 1
        endif
      endif

c     Formats

1000  format(a1)

2000  format(/' *ERROR* No previous instruction of this type.')

2001  format(/' *ERROR* No match of this macro name')

2002  format(/' Macro to be executed.'/10x,'--> ',a4,1x,a15,1p,3e12.4
     &       /' Enter y or <CR> to accept.-->',$)

2003  format(/' *ERROR* loop/next execution not permitted')

      end
