!$Id:$
      subroutine ppbox(x,y,dx,dy,is)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Draws a box with lines or fill rectangular panel

!      Inputs:
!         x,y     - Location of lower left corner of box
!         dx,dy   - Size of box
!         is      - Switch: is = 1 for filling rectangular panel
!                           is = 3 for box with outline
!                 - Sign of is used for clipping set

!      Outputs:
!         none    - Plot outputs to screen/file
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'plclip.h'

      integer       :: is
      real (kind=8) :: x, y, dx, dy

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

      end subroutine ppbox
