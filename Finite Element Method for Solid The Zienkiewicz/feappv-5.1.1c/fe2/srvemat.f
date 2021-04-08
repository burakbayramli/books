!$Id:$
      subroutine srvemat(d,eps,ta,hn,hn1,nh, sig,dd,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/05/2018
!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: RVE Constitutive Model for Small Strain Exchanges

!     Input:
!          eps(6)  -  Strain at point (small deformation)
!          ta      -  Temperature change
!          hn(nh)  -  History terms at point: t_n
!          nh      -  Number of history terms
!          isw     -  Solution option from element

!     Output:
!          hn1(nh) -  History terms at point: t_n+1
!          sig(*)  -  Stresses at point.
!                     N.B. 1-d models use only sig(1)
!          dd(6,*) -  Current material tangent moduli
!                     N.B. 1-d models use only dd(1,1) and dd(2,1)
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cdata.h'
      include   'counts.h'
      include   'debugs.h'
      include   'eldata.h'
      include   'elpers.h'
      include   'hdatam.h'
      include   'iofile.h'
      include   'sdata.h'
      include   'setups.h'
      include   'tdata.h'

      include   'mpif.h'

      include   'pointer.h'
      include   'comblk.h'

      logical       :: setval,palloc
      integer       :: nh,isw
      real (kind=8) :: ta
      real (kind=8) :: d(*),eps(6),hn(nh), hn1(nh), sig(6),dd(6,6)

!     Compute and output stress (sig) and (moduli)

      save

      if(debug) then
        call udebug('   srvemat',isw)
      endif

!     Set values of isw to send information to micro-scale problem

      if(isw.eq.14) then

!       Store material number for each send

        setval = palloc(269,'RVEMA',nsend+1, 1)
        mr(np(269)+nsend) = ma

!       Count number of sends

        sendfl = .true.
        nsend  = nsend + 1

!       Set send receive sizes

        dsend = max(dsend,15)
        drecv = max(drecv,45)

      elseif(isw.eq.4 .or. isw.eq.8) then

        sig(1:6) = hn1(1:6)

      elseif(isw.eq.3 .or. isw.eq.6 .or. isw.eq.12) then

!       Use current value of stress from array

        if(pltmfl) then

          sig(1:6) = hn1(1:6)

!       Move deformation gradient to send buffer

        elseif(sendfl) then

          rvetyp = max(1,nint(d(297)))
          call usstore(hr(np(260)),opar, n_el, dsend,nrecv, rvetyp,
     &                 eps, ta)

          if(debug) then
            call mprint(eps,1,6,1,'eps_send_4')
          endif

!         This is a set to prevent adding non-zeros to tangent/residual

          sig(:)  = 0.0d0
          dd(:,:) = 0.0d0

!       Put Cauchy stress and moduli in arrays

        elseif(recvfl) then

          call usrecv(d,hr(np(261)), drecv,nrecv, sig,dd)

!         Save stress for outputs

          if(hflgu) then
            hn1(1:6) = sig(1:6)
          endif

          if(debug) then
            call mprint(sig,1,6,1,'SIG_recv_4')
            call mprint(dd,6,6,6,'D_recv_4')
          endif

        endif ! Receive

      endif

      end subroutine srvemat

      subroutine usstore(frvel, hpar,n, dsend,nsend, rvetyp, eps, ta)

      implicit   none

      include   'iofile.h'

      integer    n,dsend,nsend, rvetyp
      real*8     hpar(*)
      real*8     ta, frvel(dsend,*), eps(6)

      save

      nsend            = nsend + 1
      frvel(1,nsend)   = n
      frvel(2,nsend)   = rvetyp
      frvel(3:8,nsend) = eps(1:6)
      frvel(12,nsend)  = 1.0d0
      frvel(13,nsend)  = ta
      frvel(14,nsend)  = hpar(1)
      frvel(15,nsend)  = hpar(2)

      end subroutine usstore

      subroutine usrecv(d,srvel, drecv,nsend, tau,ctau)

      implicit   none

      include   'debugs.h'
      include   'eldata.h'

      integer    drecv,nsend,a,b,k
      real*8     d(*),srvel(drecv,*), tau(*),ctau(6,6)

      save

      nsend = nsend + 1
      tau(1:6) = srvel(2:7,nsend)
      k = 7
      do a = 1,6
        do b = 1,6
          k = k + 1
          ctau(b,a) = srvel(k,nsend)
        end do ! b
      end do ! a

!     Set density

      d(4) = srvel(44,nsend)

      if(debug) then
        call mprint( tau,1,6,1,'SIG_rve')
        call mprint(ctau,6,6,6,'DD_rve')
      endif

      end subroutine usrecv
