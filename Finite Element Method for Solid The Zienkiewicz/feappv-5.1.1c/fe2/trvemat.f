!$Id:$
      subroutine trvemat(d, ta,thgrad, hn,hn1,nh, tflux,dd,rhoc, isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/05/2018
!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: RVE Constitutive Model for Thermal Exchanges

!     Input:
!          ta       -  Temperature
!          thgrad(3)-  Thermal gradient
!          hn(nh)   -  History terms at point: t_n
!          nh       -  Number of history terms
!          isw      -  Solution option from element

!     Output:
!          d(*)     -  Material averaged parameters
!          hn1(nh)  -  History terms at point: t_n+1
!          tflux(3) -  Flux at point.
!          dd(3,3)  -  Current material conductivity tangent moduli
!          rhoc     -  Density times specific heat
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cdata.h'
      include   'counts.h'
      include   'debugs.h'
      include   'eldata.h'
      include   'elpers.h'
      include   'hdatam.h'
      include   'iofile.h'
      include   'oelmt.h'
      include   'sdata.h'
      include   'setups.h'
      include   'tdata.h'

      include   'mpif.h'

      include   'pointer.h'
      include   'comblk.h'

      logical       :: setval,palloc
      integer       :: nh,isw
      real (kind=8) :: ta, rhoc, dum(1)
      real (kind=8) :: d(*), thgrad(3),hn(nh), hn1(nh), tflux(3),dd(3,3)

!     Compute & output flux (tflux) and (moduli)

      save

      if(debug) then
        call udebug('   trvemat',nrecv+1)
        call udebug('   trvemat',isw)
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

        dsend = max(dsend,8)
        drecv = max(drecv,17)

      elseif(isw.eq.4 .or. isw.eq.8) then

        tflux(1:3) = hn1(1:3)

      elseif(isw.eq.3 .or. isw.eq.6 .or. isw.eq.12) then

!       Use current value of stress from array

        if(pltmfl) then

          tflux(1:3) = hn1(1:3)

!       Move deformation gradient to send buffer

        elseif(sendfl) then

          rvetyp = max(1,nint(d(297)))
          call utstore(hr(np(260)),opar, n_el, dsend,nrecv, rvetyp,
     &                 thgrad, ta)

          if(debug) then
            call mprint(thgrad,1,3,1,'TGRAD_send_2')
          endif

!         This is a set to prevent adding non-zeros to tangent/residual

          thgrad(:) = 0.0d0
          dd(:,:)   = 0.0d0
          rhoc      = 0.0d0

!       Put thermal flux and moduli in arrays

        elseif(recvfl) then

          call utrecv(d, hr(np(261)), drecv,nrecv, tflux,dd, rhoc)

!         Save flux for outputs

          if(hflgu) then
            hn1(1:3) = tflux(1:3)
          endif

          if(debug) then
            call mprint(tflux,1,3,1,'FLUX_recv_2')
            call mprint(dd,3,3,3,'D_recv_2')
            dum(1) = rhoc
            call mprint(dum(1) ,1,1,1,'RHOC_recv_2')
          endif

        endif ! Receive

      endif

      end subroutine trvemat

      subroutine utstore(frvel, hn,n, dsend,nsend, rvetyp, thgrad, ta)

      implicit   none

      include   'iofile.h'

      integer    n,dsend,nsend, rvetyp
      real*8     ta, frvel(dsend,*), hn(*), thgrad(3)

      save

      nsend            = nsend + 1
      frvel(1,nsend)   = n
      frvel(2,nsend)   = rvetyp
      frvel(3:5,nsend) = thgrad(1:3)
      frvel(6,nsend)   = ta
      frvel(7,nsend)   = hn(1)
      frvel(8,nsend)   = hn(2)

      end subroutine utstore

      subroutine utrecv(d, srvel, drecv,nsend, tflux,dd, rhoc)

      implicit   none

      include   'eldata.h'

      integer    drecv,nsend,a,b,k
      real*8     d(*), srvel(drecv,*), tflux(*),dd(3,3), rhoc

      save

!     Set flux and tangent values from RVE

      nsend = nsend + 1
      do a = 1,3
        tflux(a) = -srvel(a+1,nsend)
      end do ! i
      k = 7
      do a = 1,3
        do b = 1,3
          k = k + 1
          dd(b,a) = srvel(k,nsend)
        end do ! b
      end do ! a

!     Set density and specific heat values from RVE

      d( 4) = srvel(5,nsend)
      d(64) = srvel(6,nsend)
      rhoc  = d(4)*d(64)

      end subroutine utrecv
