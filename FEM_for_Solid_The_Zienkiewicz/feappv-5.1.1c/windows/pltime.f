!$Id:$
      subroutine pltime()

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose: Place time on plot window

!      Inputs:  None

!      Outputs: To plot window
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit  none

      include  'tdata.h'
      include  'pdatxt.h'

      character (len=15) :: yy

      save

!     Display time for current view

      dtext = 0.11d0
      call pppcol(1,1)
      yy = ' '
      call tplot(1.02d0 , 0.135d0, yy, 15, 1)
      write(yy, '(6hTime =,1p,1e9.2)' ) ttim
      call tplot(1.02d0 , 0.135d0, yy, 15, 1)

      end subroutine pltime
