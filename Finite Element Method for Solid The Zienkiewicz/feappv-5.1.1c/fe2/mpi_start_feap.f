!$Id;$
      subroutine mpi_start_feap()

!      * * F E A P * * A Finite Element Analysis Program
!                        -      -       -        -
!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/05/2018
!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Initialize MPI and start all RVE processes

!     Inputs
!       None

!     Outputs
!       Starts of all files
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'comfil.h'
      include   'elpers.h'
      include   'setups.h'
      include   'mpif.h'           ! MPI common block

      character (len=128) :: sfinp,sfout,sfres,sfsav,sfplt
      character (len=3)   :: fext
      integer             :: ierr, ii, msg_stat(MPI_STATUS_SIZE)

      save

!     Initialize size of communication arrays

      dsend = 0
      drecv = 0

!     Set flag for MPI solution

      mpiflg = .true.
      rveflg = .true.

!     Initialize MPI

      call MPI_Init( ierr )

!     Get rank id for each process and total number of processes

      call MPI_Comm_rank ( MPI_COMM_WORLD, rank  , ierr)
      call MPI_Comm_size ( MPI_COMM_WORLD, ntasks, ierr)

!     write(*,*) ' Process:',rank,' of ', ntasks,' is alive'

!     Check user installation options

      call pinstall()

!     Set initial file names

      if(rank.eq.0) then

        call filnam()

        call pfe2clean()
        call pfe2outf(finp,ntasks-1)

        do ii = 1,ntasks-1
          fext = '000'
          if(ii.lt.10) then
            write(fext(3:3),'(i1)') ii
          elseif(ii.lt.100) then
            write(fext(2:3),'(i2)') ii
          else
            write(fext(1:3),'(i3)') ii
          endif
          sfinp = finp
          call addext(sfinp,fext,128,3)
          sfout = fout
          call addext(sfout,fext,128,3)
          sfres = fres
          call addext(sfres,fext,128,3)
          sfsav = fsav
          call addext(sfsav,fext,128,3)
          sfplt = fplt
          call addext(sfplt,fext,128,3)
          call  MPI_Ssend(sfinp, 128, MPI_CHARACTER,
     &                    ii, 1, MPI_COMM_WORLD, ierr)
          call  MPI_Ssend(sfout, 128, MPI_CHARACTER,
     &                    ii, 2, MPI_COMM_WORLD, ierr)
          call  MPI_Ssend(sfres, 128, MPI_CHARACTER,
     &                    ii, 3, MPI_COMM_WORLD, ierr)
          call  MPI_Ssend(sfsav, 128, MPI_CHARACTER,
     &                    ii, 4, MPI_COMM_WORLD, ierr)
          call  MPI_Ssend(sfplt, 128, MPI_CHARACTER,
     &                    ii, 5, MPI_COMM_WORLD, ierr)
        end do ! ii

      else

        call  MPI_Recv(finp, 128, MPI_CHARACTER,
     &                 0, 1, MPI_COMM_WORLD, msg_stat, ierr)
        call  MPI_Recv(fout, 128, MPI_CHARACTER,
     &                 0, 2, MPI_COMM_WORLD, msg_stat, ierr)
        call  MPI_Recv(fres, 128, MPI_CHARACTER,
     &                 0, 3, MPI_COMM_WORLD, msg_stat, ierr)
        call  MPI_Recv(fsav, 128, MPI_CHARACTER,
     &                 0, 4, MPI_COMM_WORLD, msg_stat, ierr)
        call  MPI_Recv(fplt, 128, MPI_CHARACTER,
     &                 0, 5, MPI_COMM_WORLD, msg_stat, ierr)

      endif

      end subroutine mpi_start_feap
