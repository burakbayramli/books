c$Id: pltime.f,v 1.1 2000/08/24 20:49:58 rlt Exp $
      subroutine pltime()

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--+---------+---------+---------+---------+---------+---------+-]
c      Purpose: Place time on plot window

c      Inputs:  None

c      Outputs: To plot window
c-----[--+---------+---------+---------+---------+---------+---------+-]

      implicit  none

      include  'tdata.h'
      include  'pdatxt.h'

      character yy*15

      save

c     Display time for current view

      dtext = 0.00d0
      call pppcol(1,1)
      yy = ' '
      call tplot(1.13d0 , 0.135d0, yy, 15, 1)
      write(yy, '(6hTime =,1p,1e9.2)' ) ttim
      call tplot(1.13d0 , 0.135d0, yy, 15, 1)

      end
