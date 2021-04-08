!$Id:$
      subroutine plclos()

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose: Close plot device

!      Inputs:
!         none

!      Outputs:
!         none      - Returns command outputs to text device
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit  none

      include  'iofile.h'
      include  'pdata2.h'
      include  'plflag.h'
      include  'print.h'
      include  'x11f.h'

      save

!     Close plot device

      if(.not.fopn) return
      fopn = .false.

!     X11 device

      if(screfl) call gdx11(5,xx,yy)

      end subroutine plclos
