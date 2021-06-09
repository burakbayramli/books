!$Id:$
      subroutine plstrt()

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose: Open a graphics device to receive plot data

!      Inputs:
!         none

!      Outputs:
!         none      - Graphics window should appear after call
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit  none

      include  'debugs.h'
      include  'pdata2.h'
      include  'plflag.h'
      include  'x11f.h'

      integer       :: i

      save

!     Open devices for Graphics output

      everon = .true.

!     X11 device

      idx = 440
      idy = 550

      if(screfl) call gdx11(1,xx,yy)
      if(screfl) call gdx11(7,xx,yy)
      if(debug) then
        write(*,2000) (xx(i),i = 2,6)
      endif

!     Format

2000  format('  X Length in cm.     ',f10.5/
     &       '  Y Length in cm.     ',f10.5/
     &       '  X Pixels in cm.     ',f10.5/
     &       '  Y Pixels in cm.     ',f10.5/
     &       '  No. Forground colors',f10.5/)

      end subroutine plstrt
