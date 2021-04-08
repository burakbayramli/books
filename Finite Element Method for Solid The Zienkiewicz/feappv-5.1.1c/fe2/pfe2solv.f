!$Id:$
      subroutine pfe2solv(lct)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/05/2018
!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: MPI Interface for OpenMPI: Thermo-mechanical problem
!               classes for small and finite deformation problems

!      Includes options for transient and incremental forms`

!      Problem Types:
!               1. Thermal
!               2. Stress: Small and finite deformation.

!      Inputs:
!         lct       - Command character parameters

!      Outputs:
!         N.B.  Interprocessor communications
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cdata.h'
      include   'cdat2.h'
      include   'comnds.h'
      include   'counts.h'
      include   'debugs.h'
      include   'elpers.h'
      include   'endata.h'
      include   'fdata.h'
      include   'idptr.h'
      include   'iofile.h'
      include   'ldata.h'
      include   'oelmt.h'
      include   'pglob1.h'
      include   'print.h'
      include   'prlod.h'
      include   'rdata.h'
      include   'rdat1.h'
      include   'sdata.h'
      include   'setups.h'
      include   'tdata.h'
      include   'tdato.h'

      include   'mpif.h'

      include   'p_int.h'
      include   'pointer.h'
      include   'comblk.h'
      include   'chdata.h'

      logical            :: pcomp,setval,palloc
      logical            :: flgu,flgh,hflg,ioflg
      logical            :: lflg,fflg,wflg,swapfl,startfl, exst, isopen
      logical            :: vmflg,vinput

      character (len=80) :: vvv
      character (len=15) :: lct, SWitch
      character (len=20) :: HistRD, HistWR
      character (len= 3) :: fext
      integer            :: i,j,k,nc,ni,nn,ierr
      integer            :: msg_stat(MPI_STATUS_SIZE)
      integer            :: nw, nwrv, nq, ns,nss, lenu,lenh
      integer            :: preu,preh, rtyp
      integer            :: nntot,nitot,nsends, err_no
      integer            :: rdunit,wrunit, isw, psw
      integer            :: usr_msg

      logical            :: tangfl, strefl
      real      (kind=4) :: etime, tary(2), tt
      real      (kind=8) :: volmr
      real      (kind=8) :: rbuf(26), sbuf(72), sig(6),dd(6,6), td(1)
      real      (kind=8) :: valoop,vmxits

      integer            :: idum(1)

      save

!     Start process and compute arrays for projections
      if(ntasks.le.1) then

        write(iow,*) ' *ERROR* MPI solution for np = 2 only'
        call plstop(.true.)

      elseif(pcomp(lct,'star',4)) then ! Set array for stress projects

        if(debug) then
          write(*,*) ' START FE^2 ANALYSIS: RANK =',rank
          idum(1) = ntasks
          call iprint(idum(1),1,1,1,'PFE2SOLV_NTASKS')
        endif

!       Set switch and maximum iterations for tangent computations

        call acheck(lzz(l),vvv,15,80,80)

        SWitch = vvv(1:15)
        vmflg  = vinput(vvv(16:30),15,td(1),1)
        vmxits = td(1)

        if(nint(vmxits).le.0) then
          vmxits = 15.0d0
        endif

        if(pfr) then
          write(iow,*) ' MPI: START - Parameter = ', SWitch(1:2)
     &                ,' Max TANG Iters =',nint(vmxits)
        endif

!       Set number of stress/flux components

        if(debug) then
          write(  *,*) ' PRTYPE = ',prtype
          write(iow,*) ' PRTYPE = ',prtype
        endif

        fluxfl = prtype.eq.1
        stflag = prtype.eq.2

        if(prtype.eq.1) then       ! Thermal problem
          ns     = 0
          nq     = ndm
          nw     = 15
          nsends = 17  ! Tangent + stress/flux + 2 + 1 (error)
        elseif(prtype.eq.2) then   ! Stress problem
          if(ndm.eq.1) then
            ns = 1
          elseif(ndm.eq.2) then
            ns = 4
          else
            ns = 6
          endif
          nq     = 0
          nw     = 22
          nsends = 45   ! Tangent + stress/flux + 2 + 1 (error)
        else
          write(*,*) ' --> ERROR in PFE2SOLV: prtype = 0'
          err_no = 2
        endif

!       Initialize

        hflg  = .false.

!       Set size of tangent tensor

        nss    = ns + nq              ! Size of G/H arrays
        nwrv   = nw

        setval = palloc(331,'HILLI',nen*ndf   , 1)    ! For ixl
        if(neq.gt.0) then
          setval = palloc(332,'HILLG',nss*neq*2 , 2)  ! g for stress
        endif
        setval = palloc(333,'HILLX',nen*ndm*2 , 2)    ! xs & xct coord
        call psetvol(hr(np(43)),ndm,numnp) ! Volume of RVE
        volmr  = 1.d0/volm0

        HistRD = 'HistIO_1'
        HistWR = 'HistIO_2'
        fext   = '000'
        if(rank.lt.10) then
          write(fext(3:3),'(i1)') rank
        elseif(rank.lt.100) then
          write(fext(2:3),'(i2)') rank
        else
          write(fext(1:3),'(i3)') rank
        endif
        call addext(HistRD,fext,20,3)
        call addext(HistWR,fext,20,3)
        rdunit = 3          ! History read  unit (at start)
        wrunit = 4          ! History write unit (at start)

!       Check if file exists and destroy if true

        inquire(file = HistRD, exist = exst, opened = isopen)
        if(exst) then
          if(debug) then
            write(*,*) ' PFE2SOLV: Delete existing file =',HistRD
          endif
          if(.not.isopen) then
            open (unit = rdunit, file = HistRD, status = 'old')
          endif
          close(unit = rdunit, status = 'delete')
        endif

        inquire(file = HistWR, exist = exst, opened = isopen)
        if(exst) then
          if(debug) then
            write(*,*) ' PFE2SOLV: Delete existing file =',HistWR
          endif
          if(.not.isopen) then
            open (unit = wrunit, file = HistWR, status = 'old')
          endif
          close(unit = wrunit, status = 'delete')
        endif

!       Open HistRD and HistWR files for history variable I/O

        nitot  = 0
        nntot  = 0
        psw    = 0
        ioflg  = .false.
        open(unit=rdunit, file=HistRD, form='unformatted',
     &       status='new')
        open(unit=wrunit, file=HistWR, form='unformatted',
     &       status='new')
        swapfl  = .false.
        startfl = .true.

!     Get length of U, VEL and H

        call pgetd('U    ',fp(1),lenu,preu,flgu)
        if(np(49).ne.0) then
          call pgetd('H    ',fp(2),lenh,preh,flgh)
        else
          lenh = 0
          flgh = .false.
        endif

        if(debug) write(*,*) ' LEN:',lenu,lenh

!     Kill all processes

      elseif(pcomp(lct,'stop',4)) then ! Stop all processes

        sbuf(1) = -999  ! Stop indicator
        usr_msg =  12
        do i = 1,ntasks-1
          if(debug) then
            write(*,*) 'PFE2SOLV:MPI_SSend:NSBUF,MSG',nw,usr_msg
          endif
          call MPI_SSend(sbuf, nw, MPI_DOUBLE_PRECISION, i, usr_msg,
     &                   MPI_COMM_WORLD, ierr)
        end do ! i
        write(*,*) ' --> MPI STOP EXECUTION'
        tt = etime(tary)
        write(iow,2000) tary
        call plstop(.false.)

!     Send Stress/Flux and Tangent Moduli to 'rank = 0' process

      elseif(pcomp(lct,'send',4)) then ! Send data to main processor

!       Output displacement and history record to history files

        if(rank.ge.1) then

          if(hflg) then
            if(startfl) then  ! Write to both files first time
              call pfe2setio(ni,hr(np(40)),hr(np(49)),
     &                       lenu,lenh,flgh,rdunit,2)
              if(lflg) startfl = .false.
            endif
            if(wflg) then
              call pfe2setio(ni,hr(np(40)),hr(np(49)),
     &                       lenu,lenh,flgh,wrunit,2)
              wflg = .false.
            endif
          endif

!         Compute Stress/Flux and Tangent Moduli accumulations

          if(isw.ne.12) then
            if(np(331).ne.0) then
              call psets(mr(np(331)),mr(np(33)),mr(id31),
     &                   hr(np(332)),hr(np(333)),nss, isw.eq.3)
            else
              write(iow,*) ' *ERROR* Use MPI_START in slave process'
              call plstop(.true.)
            endif

!           Set send buffer

            sbuf(1)      = ni   ! Return 'ni' stress
            if(niter.gt.0. and. flncon) err_no = 1  ! No conv flag
            sbuf(nsends) = err_no

!           Store thermal flux and moduli

            if(prtype.eq.1) then

              k = 7
              do i = 1,ndm
                sbuf(i+1) = pflux(i)*volmr
                do j = 1,ndm
                  k = k + 1
                  sbuf(k) = pcflux(j,i)*volmr
                end do ! j
              end do ! i

!             Return density and specific heat averages

              if(v_avg.gt.0.0d0) then
                sbuf(5) = v_rho/v_avg
                sbuf(6) = v_c/v_avg
              endif

!           Convert Kirchhoff stress to Cauchy stress and moduli

            elseif(prtype.eq.2) then

!             Finite deformation Cauchy stress and moduli
              if(finflg) then
                call tau2sig(ptau,pctau,volmr,fdet, sig,dd, 6)
                sbuf(2:7) = sig(1:6)
!             Small deformation Cauchy stress and moduli
              else
                sbuf(2:7) = ptau(1:6)*volmr
                dd(:,:)   = pctau(:,:)*volmr
              endif

!             Place results and moduli into send buffer
!             N.B. Stress and moduli multiplied by 'volmr'

              if(debug) call mprint(sbuf(2),1,6,1,'SIG:SEND')
              k = 7
              do i = 1,6
                do j = 1,6
                  k = k + 1
                  sbuf(k) = dd(j,i)
                end do ! j
              end do ! i

!             Element averaged density and 2-d thickness stress
              if(v_avg.gt.0.0d0) then
                sbuf(44) = v_rho/v_avg
                if(ndm.eq.2) sbuf(4) = sig_33/v_avg
              else
                sbuf(44) = 0.0d0
              endif

            endif ! prtype

          else
            sbuf(1) = rbuf(1) ! Return received value
          end if !isw

          usr_msg = 13
          if(debug) then
            write(*,*) 'PFE2SOLV:MPI_SSend:NSBUF,MSG',nsends,usr_msg
          endif
          call MPI_SSend( sbuf, nsends, MPI_DOUBLE_PRECISION, 0,
     &                    usr_msg, MPI_COMM_WORLD, ierr)
        endif

!     Get deformation gradient

      elseif(pcomp(lct,'get' ,3)) then ! Get data from Rank '0'
        nc = 0                         ! Set counter to zero
        if(rank.ge.1) then
          usr_msg = 12
          if(debug) then
            write(*,*) 'PFE2SOLV:MPI_Recv:NRBUF,MSG',nw,usr_msg
          endif
          call MPI_Recv(rbuf, nw, MPI_DOUBLE_PRECISION, 0, usr_msg,
     &                  MPI_COMM_WORLD, msg_stat, ierr)

!         Set point parameter

          ni    = nint(rbuf(1))

!         Stop execution

          if(ni.eq.-999) then
            close(unit = rdunit,status='delete')
            close(unit = wrunit,status='delete')
            tt = etime(tary)
            write(iow,2000) tary
            call plstop(.false.)  ! Stop process
          endif

!         Set control parameters

          rtyp  = nint(rbuf(2))    ! Type of RVE (1 = HM)
          nstep = nint(rbuf(3))    ! Time step indicator
          niter = nint(rbuf(4))    ! Number of iteration
          dt    = rbuf(5)          ! Time step increment
          isw   = nint(rbuf(6))    ! Switch value for computation
          hflg  = rbuf(7).eq. 1.d0 ! Flag for history  save: true =  1
          fflg  = rbuf(8).eq.-1.d0 ! Flag for first receive: true = -1
          lflg  = rbuf(9).eq. 1.d0 ! Flag for last  receive: true =  1
          wflg  = ni.gt.0          ! Set flag to write history file.
          call setparam(SWitch, rbuf(6), pfr)

          if(ni.eq.0) niter = 0

          if(isw.eq.3) then
            tangfl = .true.
            strefl = .true.
            if(niter.le.0) then
              valoop = 1.d00
              j      = lv + 1    ! Loop level for tangent loop
              if(lvs(j).gt.0) then
                ct(1,lvs(j)) = valoop
                if(debug) then
                  write(iow,*)'SET LOOP1',j,lvs(j),ct(1,lvs(j))
                endif
              endif
            else
              valoop = vmxits
              j      = lv + 1    ! Loop level for tangent loop
              if(lvs(j).gt.0) then
                ct(1,lvs(j)) = valoop
                if(debug) then
                  write(iow,*)'SET LOOP2',j,lvs(j),ct(1,lvs(j))
                endif
              endif
            endif
          else
            valoop = 1.0d0
            tangfl = .false.
            strefl = isw.eq.6
          endif

          if(debug) then
            write(iow,*) 'N =',ni,'TIME =',ttim,' NITER =',niter
            write(iow,*) 'ISW =',isw,' LOOP =',nint(valoop)
          endif

!         Initialize error number

          err_no = 0

!         Stress outputs (not needed)

          if(isw.eq.4) then

!         Receive macro model data

          else

!           Count number of total unit cells (depends on isw=6
!           being first call with a 'get'!)

            if(startfl) then
              nntot = nntot + 1
            else
              nitot = nntot
            endif

!           Swap and reopen files

            if(swapfl .or. ni.eq.0) then
              close(unit = rdunit, status='keep')
              close(unit = wrunit, status='keep')
              ioflg  = .true.
              open(unit=rdunit, file=HistRD, form='unformatted',
     &             status='old')
              open(unit=wrunit, file=HistWR, form='unformatted',
     &             status='old')
              swapfl  = .false.
            elseif(fflg) then  ! Start of new iteration
              rewind rdunit
              rewind wrunit
            endif

!           Time update

            if(isw.eq.12) then

              if(echo .and. ior.gt.0) then
                write(*,3003) nstep,ttim
              endif

!             Increment time

              ttim = ttim + dt

!             Set iteration counters

              nstep  = nstep + 1
              titer  = titer + niter
              niter  = 0
              taugm  = taugm + naugm
              naugm  = 0
              iaugm  = 0
              dtold  = dt

              do nn = 1,nitot

!               Input displacement and history

                call pfe2setio(ni,hr(np(40)),hr(np(49)),
     &                         lenu,lenh,flgh,wrunit, 1)

!               Zero displacement increment for time step

                call pzero(hr(np(40)+nneq),nneq)

!               Reset history variables and save to unit 'wrunit'

                call reshis(mr(np(33)+nen),nen1,numel,2, 1)

                call pfe2setio(ni,hr(np(40)),hr(np(49)),
     &                         lenu,lenh,flgh,rdunit,2)
              end do ! nn

              if(lflg) then
                swapfl = .true. ! Force close & reopen
              else
                rewind rdunit
                rewind wrunit
              endif
              wflg = .false.

              sbuf(1) =  ni

!           Solution step

            else
              if(nw.ge.nwrv) then

!               Input displacement and history record

                if(ioflg) then
                  call pfe2setio(ni,hr(np(40)),hr(np(49)),
     &                           lenu,lenh,flgh,rdunit, 1)
                else
                  call pzero(hr(np(40)),lenu)
                endif

!               Set gradient terms from rbuf

                call pfe2setrv(rbuf(10), ns)

!               Set edge values

                call pfe2setd(mr(id31),hr(np(43)),hr(np(27)))

              else
                write(iow,*) ' Insufficient data received in MPI_GET'
                call plstop(.true.)
              endif
            endif

          endif
        endif
      else
        write(  *,*) ' *WARNING* MPI2 ',lct(1:4),' not implemented'
        write(iow,*) ' *WARNING* MPI2 ',lct(1:4),' not implemented'
      endif

!     Formats

2000  format(' *End of Solution Execution*',31x,'t=',2f9.2)

3003  format(' --> Step',i6,' Solution: Time =',1p,1e12.5)

      end subroutine pfe2solv
