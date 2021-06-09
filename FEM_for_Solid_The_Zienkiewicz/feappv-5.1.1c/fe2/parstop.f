!$Id:$
      subroutine parstop()

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Close any open parallel array and delete memory use
!               Dummy routine in serial version.

!      Inputs:
!         none

!      Outputs:
!         none
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'debugs.h'
      include   'comfil.h'   ! finp
      include   'elpers.h'
      include   'iodata.h'   ! ios
      include   'setups.h'
      include   'mpif.h'

      character (len=128) :: ufinp
      character (len=  3) :: fext
      logical       :: lopen, lexist
      integer       :: i, ierr, nsbuf, usr_msg
      real (kind=8) :: rbuf(24)

      save

      data       rbuf / -999.0d0, 23*0.0d0 /
      data       usr_msg / 12 /

      if(debug) then
        call udebug(' parstop',0)
      endif

!     Close parallel arrays

      if(rank.eq.0) then
        nsbuf = dsend + 7
        do i = 1,ntasks - 1
          if(debug) then
            call udebug('     MPI_Send:Stop',usr_msg)
            write(*,*) 'PARSTOP:MPI_SSend:NSBUF,MSG',nsbuf,usr_msg
          endif
          call MPI_SSend(rbuf, nsbuf, MPI_DOUBLE_PRECISION, i, usr_msg,
     &                  MPI_COMM_WORLD, ierr)
        end do ! i
      endif

      if(mpiflg) then
        call MPI_Finalize( ierr )
      endif

!     Clean up input files
      do i = 1,ntasks - 1
!       Set extender for file
        fext = '000'
        if(i.lt.10) then
          write(fext(3:3),'(i1)') i
        elseif(i.lt.100) then
          write(fext(2:3),'(i2)') i
        endif

!       Input files
        ufinp = ' '
        ufinp = finp
        call addext(ufinp,fext,128,3)
        inquire(file=ufinp,opened=lopen,exist=lexist)
        if(lexist) then
          if(.not.lopen) open(unit=ios,file = ufinp)
          close(unit=ios,status='delete')
        endif
      end do ! i

      end subroutine parstop
