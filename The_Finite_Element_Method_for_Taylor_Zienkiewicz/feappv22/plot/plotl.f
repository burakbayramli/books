c$Id:$
      subroutine plotl(xx1,xx2,xx3,ipen)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Line drawing command: Converts mesh coordinates to
c               screen coordinate values

c      Inputs:
c         xx1       - X-coordinate for plot
c         xx2       - Y-coordinate for plot
c         xx3       - Z-coordinate for plot
c         ipen      - Pen position

c      Outputs:
c         none      - Plot outputs to screen/file
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'plclip.h'
      include  'pdata1.h'
      include  'ppers.h'
      include  'pview.h'

      logical   errv
      integer   ipen
      real*8    xx1,xx2,xx3, x1,x2,x3, s1,s2
      real*8    xg(3)

      save

c     Recover plot coordinates

      x1 = xx1
      x2 = xx2
      x3 = xx3

c     Perform perspective tranformation

      if(kpers.eq.1) then
        xg(1) = x1
        xg(2) = x2
        xg(3) = x3
        call perspj(xg,1,1,errv)
        if(lview) return
        x1 = xg(1)
        x2 = xg(2)
        x3 = xg(3)
      endif

c     Compute the normal coordinates


      s1 = scale*(x1 + x1 - sx(1)) + s0(1)
      s2 = scale*(x2 + x2 - sx(2)) + s0(2)

      s1 = max(0.0d0,min(1.45d0,s1))
      s2 = max(0.0d0,min(1.00d0,s2))

      clchk = .true.
      call dplot(s1,s2,ipen)
      clchk = .false.

      end
