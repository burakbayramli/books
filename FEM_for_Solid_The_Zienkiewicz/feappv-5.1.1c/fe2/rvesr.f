!$Id:$
      subroutine rvesr(frvel,srvel, ums, isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/05/2018
!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Send deformation gradient (F) or strain (eps)  to
!              micro-scale problem.
!              Receive Cauchy stress, flux, and tangent moduli.

!     Input: Mechanical Problem (prtype = 2)
!        frvel(18,*)  - Array of deformation gradients:
!                     - 1    : Element number
!                     - 2    : RVE type
!                     - 3-11 : F(1:9)
!                     - 12   : detF
!                     - 13   : ta - temperature change
!                     - 14-16: Thermal gradient
!                     - 17-18: User parameters

!        ums(ncol,*)  - Send order: ncol = ntasks - 1 (No. RVE's)
!        isw          - Element switch parameter (3, 6, or 12)

!     Output: Mechanical Problem
!        srvel(45,*)  - Array to store Cauchy stress and material moduli
!                     -  1   : Element number
!                        2-7 : Cauchy stress - sigma(1:6)
!                        8-43: Tangent moduli ctan(1:6,1:6)
!                        44  : Scalar average (rho)
!                        45  : Convergence indicator
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cdata.h'
      include   'counts.h'
      include   'debugs.h'
      include   'elpers.h'
      include   'hdatam.h'
      include   'iofile.h'
      include   'oumatl.h'
      include   'rdata.h'
      include   'sdata.h'
      include   'setups.h'
      include   'tdata.h'

      include   'mpif.h'

      logical       :: firstfl
      integer       :: isw, n,nn,mm,nsbuf,nrbuf,n_err
      integer       :: usr_msg, msg_stat(MPI_STATUS_SIZE), ierr
      integer       :: a, nproce
      integer       :: ums(ncol,nrow), idum(2)
      real (kind=8) :: frvel(dsend,*), srvel(drecv,*), sbuf(26),rbuf(72)

      save

      data       n_err  / 0 /

!     Set values

      if(debug) then
        call udebug('   rvesr',isw)
      endif

!     Set size of buffers

      nsbuf = dsend + 7
      nrbuf = drecv

!     Set fixed buffer values

      if(debug) then
        write(*,*) ' RVESR:isw,dt',isw,dt,'NSBUF=',nsbuf,nrbuf
      endif
      sbuf(3) = nstep
      sbuf(4) = niter
      sbuf(5) = dt
      sbuf(6) = isw
      if(hflgu) then
        sbuf(7) = 1.0d0  ! True
      else
        sbuf(7) = 0.0d0  ! False
      endif

      firstfl = .true.
      do nn = 1,nrow

!       Send deformation gradient to each processor

        usr_msg = 12
        do mm = 1,ncol
          n = abs(ums(mm,nn))
          if(n.gt.0) then
            sbuf(1) = nint(frvel(1,n))
            sbuf(1) = abs(n)
            sbuf(2) = nint(frvel(2,n)) ! 1 = Hill-Mandel;
            if(firstfl) then
              sbuf(8) = -1.0d0         ! First send to each processor
            else
              sbuf(8) =  0.0d0         ! Other sends to each processor
            endif
            if(ums(mm,nn).lt.0) then
              sbuf(9) = 1.0d0          ! Last send to a processor
            else
              sbuf(9) = 0.0d0          ! Other sends to a processor
            endif
            do a = 3,dsend
              sbuf(a+7) = frvel(a,n)
            end do ! a

!           Assign processor number

            nproce  = mm

            if(debug) then
              idum(1) = nproce
              idum(2) = nsbuf
              call iprint(idum(1),1,1,1,'PROCESSOR')
              call iprint(idum(2),1,1,1,'BUFFER SIZE')
              call mprint(sbuf   ,1,nsbuf,1,'SBUF')
            endif

!           Send message

            if(debug) then
              call udebug('     MPI_Send:USR_MSG',usr_msg)
              write(*,*) 'RVESR:MPI_SSend:NSBUF,MSG',nsbuf,usr_msg
            endif
            call MPI_SSend( sbuf, nsbuf, MPI_DOUBLE_PRECISION, nproce,
     &                     usr_msg,  MPI_COMM_WORLD, ierr)
            if(ierr.ne.0) then
              write(*,*) ' RVESR:IERR_send =',ierr
            endif
          endif
        end do ! mm
        firstfl = .false.

!       Receive stress and tangents from processors

        usr_msg = 13
        do mm = 1,ncol

          n = abs(ums(mm,nn))
          if(n.gt.0) then

!           Assign processor number

            nproce  = mm

!           Receive Kirchhoff stress and material moduli

            if(debug) then
              call udebug('     MPI_Recv:USR_MSG',usr_msg)
              write(*,*) 'RVESR:MPI_Recv:NRBUF,MSG',nrbuf,usr_msg
            endif
            call MPI_Recv( rbuf, nrbuf, MPI_DOUBLE_PRECISION, nproce,
     &                     usr_msg,  MPI_COMM_WORLD, msg_stat, ierr)
            if(ierr.ne.0) then
              write(*,*) ' RVESR:IERR_recv =',ierr
            endif

            if(debug) then
              idum(1) = nproce
              idum(2) = nrbuf
              call iprint(idum(1),1,1,1,'PROCESSOR')
              call iprint(idum(2),1,1,1,'BUFFER SIZE')
              call mprint(rbuf(1),1,nrbuf,1,'RBUF')
              call mprint(rbuf(11),6,6,6,'CTAU_m')
            endif

!           Store stress and tangent moduli in srvel array

            do a = 1,nrbuf
              srvel(a,n) = rbuf(a)
            end do ! a

!           Check for error: 45 = stress; 17 = thermal problem

            if(nrbuf.eq.45 .or. nrbuf.eq.17) then
              if(rbuf(nrbuf).ne.0.0d0) then
                write(iow,4001) nn,mm,n,rbuf(nrbuf)
                write(  *,4001) nn,mm,n,rbuf(nrbuf)
                n_err = n_err + 1
                if(n_err.gt.100) then
                  write(*,*) ' --> ERROR: More than 100 no convergence'
                  call plstop(.true.)
                endif
              endif
            else
              write(iow,4002) nn,nrbuf
              write(  *,4002) nn,nrbuf
              call plstop(.true.)
            endif

          endif

        end do ! mm
      end do ! nn

!     Output result of point to files for external processing

      if(isw.eq.3 .and. n_pnt.gt.0) then
         write(90,5000) ttim,(frvel(a,n_pnt),a=3,11)
         write(91,5000) ttim,(srvel(a,n_pnt),a=2,7)
         write(92,5001) ttim,(srvel(a,n_pnt),a=8,43)
      endif

!     Format

4001  format(5x,'ERROR: RVE ='i5,' PROCESSOR =',i5,' POINT',i8/
     &       5x,'       ERROR NUMBER =',1p,1e12.4)

4002  format(5x,'ERROR: RVE =',i5,' NRBUF =',i5)

5000  format(1p,10e16.8)

5001  format(1p,7e16.8/(16x,1p,6e16.8))

      end subroutine rvesr
