!$Id:$
      function propld(t,lctl)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Proportional load table (maximum 50)

!      Load types: 1)
!                  tmin = a(1)
!                  tmax = a(2)
!                  prop = a(3) + a(4)*t + a(5)*sin(a(6)*(t-tmin)+a(7))**iex
!                         for: tmin <= t <= tmax

!                  2.) Piecewise linear interpolation
!                  prop = vm(i)+(vm(i+1)-vm(i))*(t-tm(i))/(tm(i+1)-tm(i)
!                         tm(i) < t < tm(i+1)

!                  3.) Roll and slide
!                  tmin = a(1)
!                  tmax = a(2)

!                  4.) Sawtooth - cyclic loading

!                  5.) Polynomial
!                  prop - a(3) + a(4)*t + a(5)*t^2 + a(6)*t^3 + a(7)*t^4
!                  tmin = a(1)
!                  tmax = a(2)

!      Individual proportional factors also stored in /prld1/
!      Total is returned as value of function

!      Inputs:
!         t         - Time for lookup
!         lctl(2)   - Number of proportional load to define.
!                     N.B. Only used for inputs
!      Outputs:
!         propld    - Value of total proportional load
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'
      include  'p_point.h'
      include  'prlod.h'
      include  'prld1.h'
      include  'umac1.h'

      include  'pointer.h'
      include  'comblk.h'

      logical       :: pcomp, errck, pinput, setvar, palloc
      integer       :: i, j, k, l, m, nprtot, lunit
      real (kind=8) :: t, tt, propl, prop2, prop2i, prop2p

      integer   lctl(2), ilast(50),itime(50)
      real (kind=8) :: td(16)

      real (kind=8) :: propld

      save

!     Initialize propld

      propld = 0.0d0

!     Delete proportional load

      if(pcomp(uct,'dele',4)) then
        do i=lctl(1),lctl(2)

          if(ik(i).eq.2 ) then
            j = mr(np(121) + 2*i - 1)
            l = mr(np(121) + 2*i) - j
            k = 0
            do m = 1,npld
              if(ik(m).eq.2 .and. mr(np(121)+2*m-1).gt.j) then
                mr(np(121)+2*m-1) = mr(np(121)+2*m-1) - l
                mr(np(121)+2*m  ) = mr(np(121)+2*m  ) - l
                k                 = max(k,mr(np(121)+2*m))
              endif
            end do ! m
            mr(np(121))       = k               ! Reset last entry value
            mr(np(121)+2*i-1) = 0               ! Zero deleted entry
            mr(np(121)+2*i)   = 0
            do m = j,k-1
              hr(np(122)+m)   = hr(np(122)+m+l) ! Pack values
              hr(np(122)+m+1) = 0.0d0           ! Zero moved value
            end do ! m
          else
            do m = 1,7
              ap(m,i) = 0.0d0
            end do
          end if
          prldv(i) = 0.0d0
          ik(i)    = 0
          j        = npld
          do m = 1,j
            if(ik(m).gt.0) npld = m
          end do
          write(iow,3002) i,npld
          if(ior.lt.0) then
            write(*,3002) i,npld
          endif
        end do
        return
      end if

!     Input table of proportional loads

      if(lctl(1).gt.0) then
        do i=lctl(1),lctl(2)
          write(iow,2000) i
          if(ior.lt.0) then
            write(*,2000) i
            write(*,2002)
          endif

101       errck = pinput(td,9)
          if(errck) go to 101
          ik(i) = nint(td(1))
          if(ik(i).gt.0) then

!           Type 1.

            if(ik(i).eq.1) then
              iexp(i)  = nint(td(2))
              ap(1,i)  = td(3)
              ap(2,i)  = td(4)
              ap(3,i)  = td(5)
              ap(4,i)  = td(6)
              ap(5,i)  = td(7)
              ap(6,i)  = td(8)
              ap(7,i)  = td(9)

!           Type 2.

            elseif(ik(i) .eq. 2) then
              setvar = palloc(121,'PROP0',2*npld+1,1)  ! allocate table
!             Keep table clean
              do m = 1,npld
                if(ik(m).ne.2) then
                  mr(np(121)+2*m-1) = 0
                  mr(np(121)+2*m  ) = 0
                endif
              end do ! m
              if(mr(np(121)+2*i).gt.0) then
                if(ior.lt.0) then
                  write(*,3001) i
                  return
                endif
                write(iow,3001) i
                call plstop(.true.)
              else
                l      = max(1,int(td(2)))
                propl  = prop2i(lunit,l,ilast(i))
                setvar = palloc(123,'PROP2',2*ilast(i),2)
                propl  = prop2p(lunit,l,hr(np(123)),itime(i))

!               Set offset pointer into array PROP0

                nprtot            = mr(np(121))
                mr(np(121)+2*i-1) = nprtot
                mr(np(121)+2*i  ) = nprtot + 2*ilast(i)
                mr(np(121))       = nprtot + 2*ilast(i)

!               Move  PROP2 to PROP1 and destoy PROP2

                setvar   = palloc(122,'PROP1',nprtot+2*ilast(i),2)
                call pmove(hr(np(123)),hr(np(122)+nprtot),2*ilast(i))
                setvar   = palloc(123,'PROP2',0,2)

              endif

!           Error on input type

            else
              write(iow,3000) ik(i)
              if(ior.lt.0) then
                write(*,3000) ik(i)
                go to 101
              endif
              call plstop(.true.)
            endif

!         Default: Ramp loading with unit slope.

          elseif(ik(i).eq.0) then
            ap(1,i) = 0.d0
            ap(2,i) = 1.d+20
            ap(3,i) = 0.d0
            ap(4,i) = 1.d0
            ap(5,i) = 0.d0
            ap(6,i) = 0.d0
            ik(i)   = 1
          endif

!         Output proportional parameters

          if(ik(i).eq.1) then
            write(iow,2001) i,ik(i),ap(1,i),ap(2,i),
     &                      (m,m=1,5),(ap(m,i),m=3,7),iexp(i)
            if(ior.lt.0) then
              write(*,2001) i,ik(i),ap(1,i),ap(2,i),
     &                      (m,m=1,5),(ap(m,i),m=3,7),iexp(i)
            endif
          endif
        end do
      endif

!     Compute individual proportional factors at time t

      propl = 0.0d0
      do i = 1,npld

        if(t.ge.ap(1,i).and.t.le.ap(2,i) .and. ik(i).ne.2 ) then

!         Type 1. Functional proportional loading

          if(abs(ik(i)).eq.1) then
            tt       = t - ap(1,i)
            l        = max(iexp(i),1)
            prldv(i) = ap(3,i) + ap(4,i)*tt
     &               + ap(5,i)*((sin(ap(6,i)*tt+ap(7,i)))**l)

!           For 'unload' option prevent negative values

            if(ik(i).lt.0) prldv(i) = max(0.0d0,prldv(i))
            propl = propl + prldv(i)

          endif

!       Type 2. Linear interpolation loading

        elseif(ik(i).eq.2) then

          l        = 0
          point    = np(122) + mr(np(121) + 2*i-1)
          prldv(i) = prop2(t,l,hr(point),ilast(i),itime(i))
          propl    = propl + prldv(i)

!       Outside range of loadings

        else

          prldv(i) = 0.0d0

        endif

      end do

      propld = propl

!     Formats

2000  format(30x,'Proportional Load Table Number',i3)

2001  format(/,' Number    Type      Tmin',10x,'Tmax',/i3,i10,7x,g10.3,
     & 4x,g10.3,/6x,5('a(',i1,')',10x),'exp',/4x,5(g10.3,4x),i5/)

2002  format(' Input: type, exponent, tmin, tmax, a(i),i=1,4'/'   >',$)

3000  format('   Option,',i3,' does not exist: reinput data')

3001  format('  *ERROR* Proportional load table',i2,' already defined')

3002  format('  *WARNING* Deleted proportional load number',i3/
     &       '            Maximum proportional load number',i3)

      end function propld
