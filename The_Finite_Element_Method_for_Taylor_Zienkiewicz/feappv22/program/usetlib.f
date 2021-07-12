c$Id:$
      subroutine usetlib(i,prt)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Interface for user mesh manipulation set  commands

c      Inputs:
c         i      - Command number
c         prt    - Flag, output if true

c      Outputs:
c         None   - Users are responsible for providing outputs in
c                  usetmi routines
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      logical   prt
      integer   i

      save

      if(i.eq.1) then
        call usetm1(prt)
      elseif(i.eq.2) then
        call usetm2(prt)
      elseif(i.eq.3) then
        call usetm3(prt)
      elseif(i.eq.4) then
        call usetm4(prt)
      elseif(i.eq.5) then
        call usetm5(prt)
      endif

      end
