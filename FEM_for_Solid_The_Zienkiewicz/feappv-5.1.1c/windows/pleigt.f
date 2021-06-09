!$Id:$
      subroutine pleigt(eval)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose: Place eigenvalue on plot window

!      Inputs:  None

!      Outputs: To plot window
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit  none

      include  'evdata.h'
      include  'pdatxt.h'

      character (len=20) :: yy

      real (kind=8) :: eval,dd

      save

      data      yy  / ' ' /

!     Set value to plot

      dtext = 0.11d0
      call pppcol(-1,1)
      call tplot(1.02d0 , 0.160d0, yy, 20, 1)

      if(imtyp.eq.1) then
        dd = sqrt(abs(eval))*0.5d0/acos(-1.d0)
        write(yy, '(a7,1p,1e9.2,a4)' ) 'Value =',dd,' Hz.'
      else
        write(yy, '(a7,1p,1e9.2,a4)' ) 'Value =',eval,'    '
      endif

!     Display Value for current vector

      call pppcol(1,1)
      call tplot(1.02d0 , 0.160d0, yy, 20, 1)

      end subroutine pleigt
