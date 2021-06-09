!$Id:$
      subroutine ploa1(time,dt)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Calculate load level for arc length method

!      Inputs:
!         time     - Current solution time
!         dt       - Current solution time increment

!      Outputs:
!         rlnew    - Arc length load level for step
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'arclel.h'
      include  'arcler.h'

      real (kind=8) :: time,dt

      save

!     For restart only

      if (refl) then
        refl =.false.
      endif

!     Check if new time step

      if(arcf .and. time .ne. timold) then

!       Set new load level

        rlnew = rlnew + dt

      elseif(.not. arcf) then

        rlnew = 1.d0

      endif

      end subroutine ploa1
