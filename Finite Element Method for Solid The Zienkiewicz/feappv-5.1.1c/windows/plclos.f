!$Id:$
      subroutine plclos()

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose: Close plot device

!      Inputs:
!         none

!      Outputs:
!         none      - Returns command outputs to text device
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit  none

      include  'print.h'

      save

!     Close plot device

      fopn = .false.

      end subroutine plclos
