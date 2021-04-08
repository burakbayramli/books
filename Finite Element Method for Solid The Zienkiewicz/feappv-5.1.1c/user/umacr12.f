!$Id:$
      subroutine umacr12(lct,ctl)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  User interface for adding solution command language
!                instructions.

!      Inputs:
!         lct       - Command character parameters
!         ctl(3)    - Command numerical parameters

!      Outputs:
!         N.B.  Users are responsible for command actions.  See
!               programmers manual for example.
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'umac1.h'

      character (len=15) :: lct
      logical       :: pcomp
      real (kind=8) :: ctl(3)

      save

!     Set command word

      if(.not.pcomp(uct,'ma12',4)) then
!       uct = 'name'                    ! Specify 'name'
      elseif(urest.eq.1) then           ! Read  restart data

      elseif(urest.eq.2) then           ! Write restart data

      else                              ! Perform user operation

      endif

      end subroutine umacr12
