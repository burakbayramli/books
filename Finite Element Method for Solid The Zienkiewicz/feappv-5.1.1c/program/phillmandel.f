!$Id:$
      subroutine phillmandel(lct,ct)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/0520181
!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Hill-Mandel: Thermo-mechanical problem class for small
!               and finite deformation problems

!      Inputs:
!         lct       - Command character parameters
!         ct(3)     - Command numerical parameters

!      Outputs:
!         N.B.  Interprocessor communications
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cdata.h'
      include   'debugs.h'
      include   'elpers.h'
      include   'idptr.h'
      include   'iofile.h'
      include   'oelmt.h'
      include   'pglob1.h'
      include   'setups.h'
      include   'print.h'
      include   'sdata.h'

      include   'pointer.h'
      include   'comblk.h'

      character (len=15) :: lct

      logical       :: pcomp,setval,palloc
      logical       :: strefl,tangfl
      real (kind=8) :: ct(3)

      integer       :: ns,nq, nss, option, i,j
      real (kind=8) :: volmr
      real (kind=8) :: sig(6),dd(6,6)

      save

!     Check for projection type

      if(pcomp(lct,'tang',4) .or. pcomp(lct,'    ',4)) then
        tangfl = .true.
        strefl = .true.
      elseif(pcomp(lct,'stre',4)) then
        tangfl = .false.
        strefl = .true.
      else
        tangfl = .false.
        strefl = .false.
      endif

!     Set class of problem

      if(prtype.gt.0) then

!       prtype = 1      ! Thermal
!       prtype = 2      ! Mechanical

!       Set number of stress/flux components

        if(prtype.eq.1) then       ! Thermal case
          ns = 0
          nq = ndm
        elseif(prtype.eq.2) then   ! Stress solid case
          if(ndm.eq.1) then
            ns = 1
          elseif(ndm.eq.2) then
            ns = 4
          else
            ns = 6
          endif
          nq = 0
        endif

!       Set size of tangent tensor

        nss    = ns + nq          ! Size of G/H arrays

        setval = palloc(331,'HILLI',nen*ndf   , 1)    ! For ixl
        if(neq.gt.0) then
          setval = palloc(332,'HILLG',nss*neq*2 , 2)  ! g for stress
        endif
        setval = palloc(333,'HILLX',nen*ndm   , 2)    ! xs for coord
        option = nint(ct(1))
        if(v_avg.gt.0.0d0) then
          volmr = 1.d0/v_avg
          volm0 = v_avg
        else
          call psetvol(hr(np(43)),ndm,numnp) ! Volume of RVE
          volmr  = 1.d0/volm0
        endif

!       Compute Stress and Tangent Moduli accumulations

        finflg = .false.

        if(tangfl .or. strefl) then
          call psets(mr(np(331)),mr(np(33)),mr(id31),hr(np(332)),
     &               hr(np(333)), nss,tangfl)

!         Convert Kirchhoff to Cauchy stress and moduli

          if(finflg) then
            call pdetf(gradu,fdet)
            call tau2sig(ptau,pctau,volmr,fdet, sig,dd, 6)
          else
            do i = 1,nss
              sig(i) = ptau(i)*volmr
              do j = 1,nss
                dd(j,i) = pctau(j,i)*volmr
              end do ! j
            end do ! i
          endif

!         2-d thickness stress

          if(ndm.eq.2 .and. v_avg.gt.0.0d0) then
            sig(3) = sig_33/v_avg
          endif

        endif

!       Set flux

        if(prtype.eq.1) then
          pflux(:) = pflux(:)*volmr
          if(tangfl) pcflux(:,:) = pcflux(:,:)*volmr
        endif

!       Delete temp arrays

        setval = palloc(331,'HILLI',0 , 1)    ! For ixl
        if(neq.gt.0) then
          setval = palloc(332,'HILLG',0 , 2)  ! g for stress
        endif
        setval = palloc(333,'HILLX',0 , 2)    ! xs for coord

!       Output homogenized results

        if(strefl) then
          if(ior.lt.0) then
            if(prtype.eq.1) then
              write(*,2000) 'T h e r m a l    F l u x'
              write(*,2001) (pflux(j),j=1,ndm)
            else
              write(*,2000) 'C a u c h y    S t r e s s'
              write(*,2001) (sig(j),j=1,nss)
            endif
          endif
          if(prtype.eq.1) then
            write(iow,2000) 'T h e r m a l    F l u x'
            write(iow,2001) (pflux(j),j=1,ndm)
          else
            write(iow,2000) 'C a u c h y    S t r e s s'
            write(iow,2001) (sig(j),j=1,nss)
          endif
        endif

        if(tangfl) then
          if(ior.lt.0) then
            write(*,2000) 'T a n g e n t    M o d u l i'
            if(prtype.eq.1 .or. prtype.eq.4) then
              do i = 1,ndm
                write(*,2001) (pcflux(i,j),j=1,ndm)
              end do
            else
              do i = 1,nss
                write(*,2001) (dd(i,j),j=1,nss)
              end do ! i
            endif
          endif
          write(iow,2000) 'T a n g e n t    M o d u l i'
          if(prtype.eq.1) then
            do i = 1,ndm
              write(iow,2001) (pcflux(i,j),j=1,ndm)
              end do ! i
          else
            do i = 1,nss
              write(iow,2001) (dd(i,j),j=1,nss)
            end do ! i
          endif
        endif

      endif

!     Formats

2000  format(/5x,a)
2001  format(1p,6e12.4)

      end subroutine phillmandel
