c$Id:$
      subroutine ppbox(x,y,dx,dy,is)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Draws a box with lines or fill rectangular panel

c      Inputs:
c         x,y     - Location of lower left corner of box
c         dx,dy   - Size of box
c         is      - Switch: is = 1 for filling rectangular panel
c                           is = 3 for box with outline
c                 - Sign of is used for clipping set

c      Outputs:
c         none    - Plot outputs to screen/file
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'plclip.h'

      integer   is
      real*8    x, y, dx, dy

      save

      if(is.gt.0) then
        clchk = .false.
      else
        clchk = .true.
      endif

      call dplot(x   ,y   ,abs(is))
      call dplot(x+dx,y   ,2)
      call dplot(x+dx,y+dy,2)
      call dplot(x   ,y+dy,2)
      call dplot(x   ,y   ,2)
      if(abs(is).eq.1) call clpan

      clchk = .false.

      end
