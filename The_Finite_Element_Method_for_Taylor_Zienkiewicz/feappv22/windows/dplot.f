c$Id: dplot.f,v 1.1 2000/08/24 20:49:59 rlt Exp $
      subroutine dplot(x,y,ipin)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--+---------+---------+---------+---------+---------+---------+-]
c      Purpose:  Line drawing routine for device types

c      Inputs:
c         x,y       - Screen coordinates for move/draw
c         ipin      - Switch: = 1 for panel initiation; = 2 for draw;
c                             = 3 for move (no draw).

c      Outputs:
c         none      - Outputs are graphics lines/moves
c-----[--+---------+---------+---------+---------+---------+---------+-]

      use       DFLIB

      implicit  none

      integer         idxl,idyl,jfill
      common /vgraph/ idxl,idyl,jfill

      include  'plclip.h'
      include  'pdata2.h'
      include  'pdatap.h'
      include  'pdataq.h'
      include  'pdatxt.h'
      include  'pdatps.h'
      include  'plflag.h'
      include  'wdata.h'

      integer   ipen,ipin, status, vpline
      integer*2 iwx1,iwy1,iwx2,iwy2
      real*8    x, y, wmin(2),wmax(2)

      save

      data      wmin / 0.105d0, 0.895d0 /, wmax / 0.105d0, 0.895d0 /

c     Pen command motions  (ipen = 1, begin panel)
c                          (ipen = 2, drawline from to x,y)
c                          (ipen = 3, move to position x,y,
c                                     but do not draw line)
      ipen = abs(ipin)

      if(fwin .and. clchk) then
        iwx1 = int(wmin(1)*idx/idxl) + wxy(1,1,iwindow)
        iwy1 = int(wmin(2)*idy/idyl) + wxy(2,1,iwindow)
        iwx2 = int(wmax(1)*idx/idxl) + wxy(1,1,iwindow)
        iwy2 = int(wmax(2)*idy/idyl) + wxy(2,1,iwindow)
      else
        iwx1 = wxy(1,1,iwindow)
        iwy1 = wxy(2,1,iwindow)
        iwx2 = wxy(1,2,iwindow)
        iwy2 = wxy(2,2,iwindow)
      endif

c     Set current clip region size

      call setcliprgn(iwx1,iwy2,iwx2,iwy1)

c     Set flag to indicate inside clip region (or not checking)

      clip = .true.

c     Set coordinates for plot

      jx1 = x*idx
      jy1 = y*idy

      if(ipen.eq.1) then
       ixy(1,1) = jx1
       ixy(2,1) = jy1
       npf      = 1
      elseif(ipen.eq.2.and.npf.gt.0) then
       npf        = npf + 1
       ixy(1,npf) = jx1
       ixy(2,npf) = jy1
      elseif(ipen.eq.2.and.npf.le.0) then
       ixy(1,1) = jx1
       ixy(2,1) = jy1
       if(screfl) status = vpline(ixy,2)
      elseif(ipen.eq.3) then
       npf      = -1
       ixy(1,1) = jx1
       ixy(2,1) = jy1
       if(screfl) status = vpline(ixy,3)
      endif

c     Postcript

      if(hdcpy) then

        if(fwin .and. clchk) then
          if(psoff) then
            call fppsin(' cl ')
            psoff = .false.
          endif
        elseif(.not.psoff) then
          call fppsin(' fl ')
          psoff = .true.
        endif

c       Set points if necessary

        if (ipen .eq. 1) then
          xp(1) = 0.78125d0*x
          yp(1) = 0.78125d0*y
          ipan  = 1
        elseif(ipen .eq. 2) then
          if(ipan.eq.0) then
            xp(1) = xp(2)
            yp(1) = yp(2)
            xp(2) = 0.78125d0*x
            yp(2) = 0.78125d0*y
          elseif(ipan.ge.1) then
            ipan = ipan + 1
            xp(ipan) = 0.78125d0*x
            yp(ipan) = 0.78125d0*y
          endif
        elseif(ipen .eq. 3) then
          xp(1) = xp(2)
          yp(1) = yp(2)
          xp(2) = 0.78125d0*x
          yp(2) = 0.78125d0*y
        endif

c       Plot line segment

        if(ipan.eq.0 .and. ipen.eq.2) then
          call fpplps(2, xp, yp)
        endif

      endif

      end
