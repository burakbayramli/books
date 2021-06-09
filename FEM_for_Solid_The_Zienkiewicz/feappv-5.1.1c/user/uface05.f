!$Id:$
        subroutine uface05(uptyp,nel,iu,ufac)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set plot data for user element

!      Inputs:
!         uptyp   - User element topology
!         nel     - Number of element nodes

!      Output:
!         iu(4,*) - 4-node quadrilateral face
!         ufac    - Number of faces
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      integer        :: uptyp,nel,ufac
      integer        :: iu(4,*)

      end subroutine uface05
