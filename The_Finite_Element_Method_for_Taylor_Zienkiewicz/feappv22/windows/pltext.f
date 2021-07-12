c$Id: pltext.f,v 1.1 2000/08/24 20:49:59 rlt Exp $
      subroutine pltext(x,y,il,str)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--+---------+---------+---------+---------+---------+---------+-]
c      Purpose: Place graphics text at screen positions

c      Inputs:
c         x,y       - Screen coordinates for graphics text
c         il        - Length of text
c         str(*)    - Text string to place

c      Outputs:
c         none      - Plot outputs to screen/file
c-----[--+---------+---------+---------+---------+---------+---------+-]

      implicit  none

      include  'pdatxt.h'

      character str(*)*1
      integer   il
      real*8    x,y

      save

c     Set coordinates for plot

      dtext = 0.00d0

      call tplot(x,y,str,il,0)

      end
