!$Id;$
      subroutine mpi_start_feap()

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/11/2016
!       1. Add set of rveflg to false                       26/01/2018
!       2. Change name to mpi_start_feap                    09/04/2018
!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Initialize MPI and start all processes

!     Inputs:
!       MPI
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'setups.h'

!     Dummy routine for serial program

      mpiflg = .false. ! Ensure flag is false for ntask = 1
      rveflg = .false. ! Ensure flag is false for ntask = 1

      end subroutine mpi_start_feap
