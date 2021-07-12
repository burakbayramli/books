c$Id: tplot.f,v 1.1 2000/08/24 20:49:59 rlt Exp $
      subroutine tplot(x1,y1,tx,mm,nctr)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--+---------+---------+---------+---------+---------+---------+-]
c      Purpose: Place a string of text in plot

c      Inputs:
c         x1,y1     - Postion on plot to place text
c         tx(*)     - String of text to plot
c         mm        - Number of characters in text
c         nctr      - Centering information

c      Outputs:
c         none      - Text placed in plot region on screen/file
c-----[--+---------+---------+---------+---------+---------+---------+-]

      implicit  none

      include  'pdatap.h'
      include  'pdatps.h'
      include  'psdat3.h'
      include  'pdatxt.h'
      include  'plflag.h'

      integer   mm,nn
      character tx(*)*1
      integer   i, nctr, vgtxts
      real*8    x1,y1

      save

c     Set number of characters

      nn = abs(mm)

c     Position for text output

      if(screfl) i = vgtxts(x1,y1+0.03d0,nn,tx)

c     PostScript: Output text

      if (hdcpy) then
        call dplot(x1+dtext,y1,3)
        call fptplt(xp,yp,tx,nn,nctr)
      endif

      end
