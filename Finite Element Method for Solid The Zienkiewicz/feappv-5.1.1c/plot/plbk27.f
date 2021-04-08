!$Id:$
      subroutine plbk27(iel)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set 3-D Plot Sequence for 27-node brick elements

!      Inputs:
!         iel       - Element number: > 0 for user    elements
!                                     < 0 for program elements

!      Outputs:
!         none      - Sequesnce returned in common /pdata6/
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'pdata5.h'
      include  'pdata6.h'

      integer      :: iel

      save

!     Set number of points

      if(iel.gt.0) then

        inord(iel)    = 32

!       Set plot sequence

        ipord( 1,iel) =  1
        ipord( 2,iel) =  9
        ipord( 3,iel) =  2
        ipord( 4,iel) = 10
        ipord( 5,iel) =  3
        ipord( 6,iel) = 11
        ipord( 7,iel) =  4
        ipord( 8,iel) = 12
        ipord( 9,iel) =  1
        ipord(10,iel) = 17
        ipord(11,iel) =  5
        ipord(12,iel) = 13
        ipord(13,iel) =  6
        ipord(14,iel) = 18
        ipord(15,iel) =  2
        ipord(16,iel) = 18
        ipord(17,iel) =  6
        ipord(18,iel) = 14
        ipord(19,iel) =  7
        ipord(20,iel) = 19
        ipord(21,iel) =  3
        ipord(22,iel) = 19
        ipord(23,iel) =  7
        ipord(24,iel) = 15
        ipord(25,iel) =  8
        ipord(26,iel) = 20
        ipord(27,iel) =  4
        ipord(28,iel) = 20
        ipord(29,iel) =  8
        ipord(30,iel) = 16
        ipord(31,iel) =  5
        ipord(32,iel) = 17

      elseif(iel.lt.0) then

        exord(-iel)    = 32

!       Set plot sequence

        epord( 1,-iel) =  1
        epord( 2,-iel) =  9
        epord( 3,-iel) =  2
        epord( 4,-iel) = 10
        epord( 5,-iel) =  3
        epord( 6,-iel) = 11
        epord( 7,-iel) =  4
        epord( 8,-iel) = 12
        epord( 9,-iel) =  1
        epord(10,-iel) = 17
        epord(11,-iel) =  5
        epord(12,-iel) = 13
        epord(13,-iel) =  6
        epord(14,-iel) = 18
        epord(15,-iel) =  2
        epord(16,-iel) = 18
        epord(17,-iel) =  6
        epord(18,-iel) = 14
        epord(19,-iel) =  7
        epord(20,-iel) = 19
        epord(21,-iel) =  3
        epord(22,-iel) = 19
        epord(23,-iel) =  7
        epord(24,-iel) = 15
        epord(25,-iel) =  8
        epord(26,-iel) = 20
        epord(27,-iel) =  4
        epord(28,-iel) = 20
        epord(29,-iel) =  8
        epord(30,-iel) = 16
        epord(31,-iel) =  5
        epord(32,-iel) = 17

      endif

      end subroutine plbk27
