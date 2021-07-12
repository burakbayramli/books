c$Id:$
      subroutine pltri6(iel)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Set 3-d Plot Sequence for 6-node triangular elements

c      Inputs:
c         iel       - Element type number

c      Outputs:
c         none      - Plot outputs to screen/file
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'pdata5.h'
      include  'pdata6.h'

      integer   iel

      save

c     Set number of points

      if(iel.gt.0) then

        inord(iel)    = 7

c       Set plot sequence

        ipord( 1,iel) = 1
        ipord( 2,iel) = 4
        ipord( 3,iel) = 2
        ipord( 4,iel) = 5
        ipord( 5,iel) = 3
        ipord( 6,iel) = 6
        ipord( 7,iel) = 1

      elseif(iel.lt.0) then

        exord(-iel)    = 7

c       Set plot sequence

        epord( 1,-iel) = 1
        epord( 2,-iel) = 4
        epord( 3,-iel) = 2
        epord( 4,-iel) = 5
        epord( 5,-iel) = 3
        epord( 6,-iel) = 6
        epord( 7,-iel) = 1

      endif

      end
