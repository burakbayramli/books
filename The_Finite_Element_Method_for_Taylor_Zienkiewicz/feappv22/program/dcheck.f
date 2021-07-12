c$Id:$
      subroutine dcheck(x,vd,nt,error)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Internal input of values from string data

c      Inputs:
c         x(*)  - Character array from inputs
c         nt    - Length of character string

c      Outputs:
c         vd(*) - Numerical values from string inputs
c         error - True of error occurs during inputs
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'conval.h'
      include  'iofile.h'

      logical   error
      character x*(*),y*255
      integer   nt,n0,nn, i, ivd(2,16)
      real*8    vd(*)

      save

c     Expression substitution using predefined constants

      n0 = 15
      nn = 1
      do i = 1,nt
        if(x(i:i).eq.',') nn = nn + 1
      end do ! i
      call acheck(x,y,n0,nt,255)
      call pcharr(y,ivd,n0,nn*n0)
      error = .false.
      read(y,'(17f15.0)',err=200) (vd(i),i=1,nn)
      do i = 1,nn
        if(ivd(1,i).gt.0) then
          vd(i) = vvv(ivd(1,i),ivd(2,i))
        elseif(ivd(1,i).lt.0) then
          vd(i) = www(-ivd(1,i))
        endif
      end do ! i
      return

c     Error

 200  write(iow,3000) nn,y(1:nn*15)
      if(ior.lt.0) then
        write(  *,3000) nn,y(1:nn*15)
      endif
      call errclr('INPUTS')
      error = .true.

3000  format(/' *ERROR* DCHECK: Attempt to input',i3,' value(s).',
     &        '  Input is:'/a)

      end
