c$Id: plstop.f,v 1.1 2000/08/24 20:49:59 rlt Exp $
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

      use       DFLIB

      implicit  none

      include  'pdatps.h'
      include  'plflag.h'

      integer   status,vclrwk,vclswk

      save

c     Close PostScript file if open

      if (hdcpy) call fpplcl()

      if(everon) then
        status = vclrwk()
        status = vclswk()
      endif

c     Clear last time history plot

      call ptimpl()

      status = setexitqq(QWIN$EXITNOPERSIST)


      stop

      end
