c$Id: clpan.f,v 1.1 2000/08/24 20:49:59 rlt Exp $
      subroutine clpan()

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--+---------+---------+---------+---------+---------+---------+-]
c      Purpose: Close panel plots

c      Inputs:
c         none

c      Outputs:
c         none
c-----[--+---------+---------+---------+---------+---------+---------+-]

      implicit  none

c     include  'pdata2.h'
      include  'pdatap.h'
      include  'pdatps.h'
      include  'pdataq.h'
      include  'plflag.h'
      include  'psdat3.h'

      integer   status, vfarea

      save

c     Close panel for filled plots

      if(screfl) status = vfarea(npf,ixy)
      npf    = 0

c     Fill panel for PostScript

      if (hdcpy .and. ipan .ge. 3 ) then
        call fppspl(ipan,xp,yp)
      endif

c     Reinitialize panel counter

      ipan = 0

      end
