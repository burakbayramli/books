c$Id:$
      subroutine umesh3(prt)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Dummy user input routine

c      Inputs:
c         prt    - Flag, output results if true

c      Outputs:
c         none   - Users are responsible for generating outputs
c                  through common blocks, etc.  See programmer
c                  manual for example.
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'umac1.h'

      logical   prt,pcomp

c     Set command

      if(pcomp(uct,'mes3',4)) then      ! Usual    form
c       uct = 'name'                    ! Specify 'name'
      elseif(ucount) then               ! Count elements and nodes

      elseif(urest.eq.1) then           ! Read  restart data

      elseif(urest.eq.2) then           ! Write restart data

      else                              ! Perform user operation

      endif

      end
