c$Id:$
      subroutine plbrk8(iel)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Set 3-D Plot Sequence for 8-node brick elements

c      Inputs:
c         iel       - Element number: > 0 for user    elements
c                                     < 0 for program elements

c      Outputs:
c         none      - Sequesnce returned in common /pdata6/
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'pdata5.h'
      include  'pdata6.h'

      integer   iel

      save

c     Set number of points

      if(iel.gt.0) then

        inord(iel)    = 16

c       Set plot sequence

        ipord( 1,iel) = 1
        ipord( 2,iel) = 2
        ipord( 3,iel) = 3
        ipord( 4,iel) = 4
        ipord( 5,iel) = 1
        ipord( 6,iel) = 5
        ipord( 7,iel) = 6
        ipord( 8,iel) = 2
        ipord( 9,iel) = 6
        ipord(10,iel) = 7
        ipord(11,iel) = 3
        ipord(12,iel) = 7
        ipord(13,iel) = 8
        ipord(14,iel) = 4
        ipord(15,iel) = 8
        ipord(16,iel) = 5

      elseif(iel.lt.0) then

        exord(-iel)    = 16

c       Set plot sequence

        epord( 1,-iel) = 1
        epord( 2,-iel) = 2
        epord( 3,-iel) = 3
        epord( 4,-iel) = 4
        epord( 5,-iel) = 1
        epord( 6,-iel) = 5
        epord( 7,-iel) = 6
        epord( 8,-iel) = 2
        epord( 9,-iel) = 6
        epord(10,-iel) = 7
        epord(11,-iel) = 3
        epord(12,-iel) = 7
        epord(13,-iel) = 8
        epord(14,-iel) = 4
        epord(15,-iel) = 8
        epord(16,-iel) = 5

      endif

      end
