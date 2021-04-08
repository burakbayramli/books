!$Id:$
      subroutine parsend()

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    12/21/2007
!       1. Revised form for multiple RVE mesh types         12/05/2010
!       2. Add sbuf(8) increase send size                   13/12/2010
!       3. Increase storage for rbuf to 24                  20/07/2012
!       4. Replace 'omacr1.h' by 'elpers.h'                 21/05/2013
!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: MPI Time update Exchanges

!     Input:
!       None

!     Output:
!       None
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'debugs.h'
      include   'counts.h'
      include   'elpers.h'
      include   'hdatam.h'
      include   'iofile.h'
      include   'setups.h'
      include   'tdata.h'

      include   'mpif.h'

      integer       :: mm,nsbuf,nrbuf
      integer       :: usr_msg, msg_stat(MPI_STATUS_SIZE), ierr
      integer       :: nproce

      real (kind=8) :: sbuf(26), rbuf(91)

!     Compute time update

      save

!     Set values

!     Exit for single task problems

      if(ntasks.le.1) then

        return

!     Send: Time update from Rank 0 model to each task

      elseif(rank.eq.0) then

!       Set size of buffers

        nsbuf = dsend + 7
        nrbuf = drecv

        sbuf(1) = 0           ! Update number
        sbuf(2) = rvetyp      ! From setups.h
        sbuf(3) = nstep       ! From /counts/
        sbuf(4) = niter       ! From /counts/
        sbuf(5) = dt          ! From /tdata/
        sbuf(6) = 12          ! ISW element switch value
        sbuf(7) = 1.d0        ! (1 = Fortran true)
        sbuf(8) = 0.0d0
        sbuf(9) = 0.0d0

!       Send deformation gradient to each processor

        usr_msg = 12
        do mm = 1,ntasks-1

!         Assign processor number

          nproce  = mm

!         Send message

          if(debug) then
            call udebug('     MPI_SSend:Tgrad',usr_msg)
            write(*,*) 'PARSEND:MPI_SSend:NSBUF,MSG',nsbuf,usr_msg
          endif
          call MPI_SSend( sbuf, nsbuf, MPI_DOUBLE_PRECISION, nproce,
     &                   usr_msg,  MPI_COMM_WORLD, ierr)
          if(ierr.ne.0) then
            write(*,*) ' IERR_send =',ierr
          endif
        end do ! mm

!       Receive message from processors

        usr_msg = 13
        do mm = 1,ntasks-1

!         Assign processor number

          nproce  = mm

!         Receive time update reply

          if(debug) then
            call udebug('     MPI_Recv:Flux',usr_msg)
            write(*,*) 'PARSEND:MPI_Recv:NRBUF,MSG',nrbuf,usr_msg
          endif
          call MPI_Recv( rbuf, nrbuf, MPI_DOUBLE_PRECISION, nproce,
     &                   usr_msg,  MPI_COMM_WORLD, msg_stat, ierr)

        end do ! mm

      endif

      end subroutine parsend
