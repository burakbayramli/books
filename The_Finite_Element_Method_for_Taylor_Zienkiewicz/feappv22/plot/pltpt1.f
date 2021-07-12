c$Id:$
      subroutine pltpt1(iel)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Set 3-d Plot Sequence for 1-node point elements

c      Inputs:
c         iel       - Element type number

c      Outputs:
c         none      - Output through common block data
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'pdata5.h'
      include  'pdata6.h'

      integer   iel

      save

c     Set number of points

      if(iel.gt.0) then

        inord(iel)    = -1

      elseif(iel.lt.0) then

        exord(-iel)    = -1

      endif

      end
