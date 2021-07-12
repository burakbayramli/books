c$Id:$
      subroutine umaclib(i,lct,ct,prt)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Interface for user command language instructions

c      Inputs:
c         i      - Command number
c         lct    - Character array describing option
c         ct(3)  - Command parameters
c         prt    - Output if true

c      Outputs:
c         N.B.  Users are responsible for generating command options
c               See programmer manual for example.
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      logical   prt
      character lct*(*)
      integer   i
      real*8    ct(3)

      save

      if(    i.eq.1) then
        call umacr1(lct,ct,prt)
      elseif(i.eq.2) then
        call umacr2(lct,ct,prt)
      elseif(i.eq.3) then
        call umacr3(lct,ct,prt)
      elseif(i.eq.4) then
        call umacr4(lct,ct,prt)
      elseif(i.eq.5) then
        call umacr5(lct,ct,prt)
      endif

      end
