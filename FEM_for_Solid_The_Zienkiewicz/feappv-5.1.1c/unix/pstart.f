!$Id:$
      subroutine pstart()
!
!      * * F E A P * * A Finite Element Analysis Program
!                        -      -       -        -
!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved
      implicit      none

      include  'codat.h'
      include  'pdata2.h'
      include  'setups.h'

      save

!     Set flags for seial execution
      ntasks = 1
      rank   = 0

!     Start for X11 graphics driver

      idev = 1

!     File input from command line

      fileck = .true.

!     Start versions that use MPI

      mpiflg = .false.
      call mpi_start_feap()

!     Start other versions

      if(.not.mpiflg) then

!       Check user installation options

        call pinstall()

!       Set all filenames

        call filnam()

      endif

      end subroutine pstart
