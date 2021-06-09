!$Id:$
      subroutine umacr3(lct,ctl)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Compare nodal displacements computed by pdblock.

!      Inputs:
!         lct       - Command character parameters
!         ctl(3)    - Command numerical parameters

!      Outputs:
!         Output is error data for boundary displacements
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'
      include  'umac1.h'

      logical   pcomp
      character lct*15
      real*8    ctl(3)

      save

!     Set command word

      if(pcomp(uct,'mac3',4)) then      ! Usual    form
        uct = 'bdis'                    ! Specify 'name'
      elseif(urest.eq.1) then           ! Read  restart data

      elseif(urest.eq.2) then           ! Write restart data

      else                              ! Perform user operation

        call pdblock(3)

      endif

      end
