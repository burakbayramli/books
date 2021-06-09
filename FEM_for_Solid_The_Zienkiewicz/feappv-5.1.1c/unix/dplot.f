!$Id:$
      subroutine dplot(x,y,ipin)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose:  Line drawing routine for device types

!      Inputs:
!         x,y       - Screen coordinates for move/draw
!         ipin      - Switch: = 1 for panel initiation; = 2 for draw;
!                             = 3 for move (no draw).

!      Outputs:
!         none      - Outputs are graphics lines/moves
!-----[--+---------+---------+---------+---------+---------+---------+-]

      implicit  none

      include  'plclip.h'
      include  'pdata2.h'
      include  'pdatap.h'
      include  'pdataq.h'
      include  'pdatxt.h'
      include  'pdatps.h'
      include  'plflag.h'
      include  'x11f.h'

      integer       :: ipen,ipin
      real (kind=4) :: x11, y11, xtem, ytem
!     real (kind=4) :: xx1(4),yy1(4)
      real (kind=8) :: x, y

      save

!     Pen command motions  (ipen = 1, begin panel)
!                          (ipen = 2, drawline from to x,y)
!                          (ipen = 3, move to position x,y,
!                                     but do not draw line)
      ipen = abs(ipin)

      if(fwin .and. clchk) then
!       call pclip(x,y,ipin)
!       if(fwoff) then
!         xx1(1) = 0.1
!         yy1(1) = 0.1
!         xx1(2) = 1.1
!         yy1(2) = 1.1
!         xx1(3) = 10.0
!         call gdx11(14,xx1,yy1)
!         fwoff = .false.
!       endif
!     elseif(.not.fwoff) then
!       xx1(3) =  0.0
!       call gdx11(14,xx1,yy1)
!       fwoff = .true.
      else
!     endif

!       Set flag to indicate inside clip region (or not checking)

        clip = .true.

        xtem = real(0.78125d0*x)
        ytem = real(0.78125d0*y)

!       X11 device

        if (ipen .eq. 1) then
          xp(1) = xtem
          yp(1) = ytem
          ipan  = 1
        elseif(ipen .eq. 2) then
          if(ipan.eq.0) then
            xp(1) = xp(2)
            yp(1) = yp(2)
            xp(2) = xtem
            yp(2) = ytem
            x11 = xp(1)*xx(2)
            y11 = yp(1)*xx(3)*1.28
            if(screfl) then
              call gdx11(3,x11,y11)
            endif
            x11 = xp(2)*xx(2)
            y11 = yp(2)*xx(3)*1.28
            if(screfl) then
              call gdx11(4,x11,y11)
            endif
          elseif(ipan.ge.1) then
            ipan = ipan + 1
            xp(ipan) = xtem
            yp(ipan) = ytem
          endif
        elseif(ipen .eq. 3) then
          xp(1) = xp(2)
          yp(1) = yp(2)
          xp(2) = xtem
          yp(2) = ytem
        endif

!       Postcript

        if(hdcpy) then

!         Plot line segment

          if(ipan.eq.0 .and. ipen.eq.2) then
            call fpplps(2, xp, yp)
          endif

        endif

      endif

      end subroutine dplot
