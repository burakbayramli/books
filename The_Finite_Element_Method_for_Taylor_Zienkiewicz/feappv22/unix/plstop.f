c$Id: plstop.f,v 1.1 2000/08/24 20:49:58 rlt Exp $
      subroutine plstop()

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--+---------+---------+---------+---------+---------+---------+-]
c      Purpose: Close any open plot windows and stop execution

c      Inputs:
c         none

c      Outputs:
c         none
c-----[--+---------+---------+---------+---------+---------+---------+-]

      implicit  none

      include  'pdata2.h'
      include  'pdatps.h'
      include  'plflag.h'
      include  'x11f.h'

      save

c     Close PostScript file if open

      if (hdcpy) call fpplcl()

c     X11 device

      if(everon) call gdx11(6,xx,yy)

c     Clear last time history plot data set

      call ptimpl()

      stop

      end
