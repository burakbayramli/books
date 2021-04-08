!$Id:$
      subroutine uprob

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/05/2018
!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: MPI - Receive command to start mesh on micro problems
!               Allows for different RVE's on each processor.

!      Called from pcontr for input files beginning 'ufeap' or 'fe2feap'

!      Inputs:
!         none

!      Outputs:
!         none   - Users are responsible for generating problems
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cblktr.h'
      include   'cdata.h'
      include   'cdat2.h'
      include   'comfil.h'
      include   'debugs.h'
      include   'elpers.h'
      include   'iofile.h'
      include   'mxsiz.h'
      include   'sdata.h'
      include   'setups.h'
      include   'pointer.h'
      include   'comblk.h'

      include   'mpif.h'

      logical             :: setvar, pcomp, vinput
      character (len=132) :: sbuf
      character (len=128) :: filenam
      character (len= 15) :: uvalue
      integer             :: i, ierr, usr_msg
      integer             :: msg_stat(MPI_STATUS_SIZE)
      real       (kind=8) :: vv(2)

      save

      if(debug) then
        call udebug(' uprob',0)
      endif

!     Set command

      if(rank.gt.0) then

!       Receive file name from macro program

        usr_msg = 14

        if(debug) then
          call udebug('     MPI_Recv:NEW',usr_msg)
          write(*,*) 'UPROB:MPI_Recv:MSG',usr_msg
        endif
        call MPI_Recv(sbuf, 132, MPI_CHARACTER, 0, usr_msg,
     &                  MPI_COMM_WORLD, msg_stat, ierr)
        filenam  = sbuf(1:128)
        uvalue   = ' '
        uvalue(14:15) = sbuf(129:130)
        setvar   = vinput(uvalue,15,vv(1),1)
        uvalue   = ' '
        uvalue(14:15) = sbuf(131:132)
        setvar = vinput(uvalue,15,vv(2),1)

!       Set findex to use for file offsets

        findex = 1
        do i = len_trim(filenam),1,-1
          if(pcomp(filenam(i:i),char(47),1) .or.       ! char(47) = '/'
     &       pcomp(filenam(i:i),char(92),1)) go to 100 ! char(92) = '\'
        end do ! i
        i = 0
100     findex = i + 1

!       Set inputs from file specified in second field

        call pincld(filenam)

!       Start problem

        nio = 0
        neo = 0
        mao = 0
        call pnewprob(0)

        call pincld('end')

!       Export problem type and deformation mode

        prtype = nint(vv(1))
        finflg = vv(2).le.0.0d0

      endif

      end subroutine uprob
