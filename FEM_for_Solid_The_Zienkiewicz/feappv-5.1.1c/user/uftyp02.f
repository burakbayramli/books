!$Id:$
        subroutine uftyp02(uptyp,nel,iel)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set plot data for user element

!      Inputs:
!         uptyp   - User element topology
!         nel     - Number of element nodes
!         iel     - Element routine number

!      Output:
!         inord   - Number of plot
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'pdata6.h'

      integer        :: uptyp,nel,iel

!     Plot for perspective contours: Do not change

      if(nel.eq.4) then

        inord(iel) = 5

        ipord( 1,iel) = 1
        ipord( 2,iel) = 2
        ipord( 3,iel) = 4
        ipord( 4,iel) = 3
        ipord( 5,iel) = 1

!     Plot for mesh: Set inord(iel) and ipord(i,iel)

      else


      endif

      end subroutine uftyp02
