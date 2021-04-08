!$Id:$
      subroutine uglobl(vtype,td)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Dummy user global parameter routine

!      Inputs:
!         vtype(2)  - Character array describing user global command
!         td(5)     - Real array of data for global command

!      Outputs:
!         N.B. Users must provide output via common blocks, etc.
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      character (len=15) :: vtype(2)

      real (kind=8) :: td(5)

      end subroutine uglobl
