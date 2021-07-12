c$Id: pleigt.f,v 1.1 2000/08/24 20:49:58 rlt Exp $
      subroutine pleigt(eval)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--+---------+---------+---------+---------+---------+---------+-]
c      Purpose: Place eigenvalue on plot window

c      Inputs:  None

c      Outputs: To plot window
c-----[--+---------+---------+---------+---------+---------+---------+-]

      implicit  none

      include  'evdata.h'
      include  'pdatxt.h'

      character yy*20
      real*8    eval,dd

      save

      data      yy  / ' ' /

c     Set value to plot

      dtext = 0.00d0
      call pppcol(-1,1)
      call tplot(1.13d0 , 0.160d0, yy, 20, 1)

      if(imtyp.eq.1) then
        dd = sqrt(abs(eval))*0.5d0/acos(-1.d0)
        write(yy, '(a7,1p,1e9.2,a4)' ) 'Value =',dd,' Hz.'
      else
        write(yy, '(a7,1p,1e9.2,a4)' ) 'Value =',eval,'    '
      endif

c     Display Value for current vector

      call pppcol(1,1)
      call tplot(1.13d0 , 0.160d0, yy, 20, 1)

      end
