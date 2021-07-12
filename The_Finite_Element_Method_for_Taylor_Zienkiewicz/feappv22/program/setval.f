c$Id:$
      subroutine setval(xi,num, val)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Represent character constants by free inputs in strings

c      Inputs:
c         xi        - Input string
c         num       - Length of character string
c      Outputs:
c         val       - Value of string
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'codat.h'
      include  'conval.h'
      include  'errchk.h'
      include  'iofile.h'

      logical   errco
      integer   i, nex, num

      character xt*75,xs*75,xi*(*)
      real*8    val, v(25)

      save

c     Read value if no constants have been set

      if(.not.coflg) then
        xs(1:15) = ' '
        nex      = 15 - num
        do i = 1,num
          xs(i+nex:i+nex) = xi(i:i)
        end do
        errco = .false.
        call pinval(xs,val,errco)
        if(errco) go to 60
        return
      endif

c     Find letter number for this parameter

60    xs = ' '
      xs(1:num) = xi(1:num)

c     Evaluate expression

1     nex = 0
      errck = .false.
      call parexp(xs,xt,v,nex,errck)
      if(errck) go to 150

      call pfuncs(xs,v,val,nex,errck)
      if(errck) go to 150
      return

c     An error has been detected in statement respecify

150   if(ior.lt.0) then
        write(*,2001) xi(1:num)
151     read (*,1000,err=152,end=153) xt
        goto  154
152     call  errclr ('SETVAL')
        goto  151
153     call  endclr ('SETVAL',xt)
154     write(iow,2002) xt
        call pcheck(1,xt,errck)
        xs( 1:74) = xt(1:74)
        xs(75:75) = ' '
        errck = .false.
        go to 1

c     Error on a read

      else
        call  errclr ('SETVAL')
      endif

c     Formats

 1000 format(75a1)

 2001 format(2x,a1,' = ',74a1/'   >',$)

 2002 format('  Correction:>',75a1)

      end
