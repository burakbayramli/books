c$Id: plclos.f,v 1.1 2000/08/24 20:49:58 rlt Exp $
      subroutine plclos()

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--+---------+---------+---------+---------+---------+---------+-]
c      Purpose: Close plot device

c      Inputs:
c         none

c      Outputs:
c         none      - Returns command outputs to text device
c-----[--+---------+---------+---------+---------+---------+---------+-]

      implicit  none

      include  'iofile.h'
      include  'pdata2.h'
      include  'plflag.h'
      include  'print.h'
      include  'x11f.h'

      save

c     Close plot device

      if(.not.fopn) return
      fopn = .false.

c     X11 device

      if(screfl) call gdx11(5,xx,yy)

      end
