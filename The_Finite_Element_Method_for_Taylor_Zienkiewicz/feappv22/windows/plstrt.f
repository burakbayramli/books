c$Id: plstrt.f,v 1.1 2000/08/24 20:49:59 rlt Exp $
      subroutine plstrt()

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--+---------+---------+---------+---------+---------+---------+-]
c      Purpose: Open a graphics device to receive plot data

c      Inputs:
c         none

c      Outputs:
c         none      - Graphics window should appear after call
c-----[--+---------+---------+---------+---------+---------+---------+-]

      use       DFLIB

      implicit  none

      include  'pdata2.h'
      include  'plflag.h'
      include  'wdata.h'

      integer   status

      save

c     Open devices for Graphics output

      everon = .true.

      call clearscreen($GCLEARSCREEN)
      status = displaycursor($GCURSOROFF)

      end
