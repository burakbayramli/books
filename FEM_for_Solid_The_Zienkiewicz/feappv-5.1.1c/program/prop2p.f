!$Id:$
      function prop2p(lunit,l,tv,itime)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Proportional load table type 2 input/computation

!                 prop = vm(i)+(vm(i+1)-vm(i))*(t-tm(i))/(tm(i+1)-tm(i)
!                        tm(i) < t < tm(i+1)
!      Inputs:
!         l         - Number of data input pairs to input/record
!                     Compute proportional load if zero.

!      Outputs:
!         prop2     - Value of total proportional load type 2
!         tv(2,*)   - Table of times and values:
!                       tm(*) = tv(1,*); vm(*) = tv(2,*)
!         itime     - Activation indicator
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'comfil.h'
      include  'iodata.h'
      include  'iofile.h'
      include  'print.h'

      logical       :: errck, pinput, done
      integer       :: l, m, ilast, itime, lunit, iosav
      real (kind=8) :: prop2p, tv(2,*), td(16)

      save

!     Input table of proportional loads

      ilast = 0
      itime = 1

!     Rewind file to permit setting the tv(2,*) array

      rewind lunit
      iosav = ior
      ior   = lunit

!     Start read

      done = .false.
      do while(.not.done)
102     errck = pinput(td,2*l)
        if(errck) go to 102
        do m = 1,l
          if(abs(td(2*m-1))+abs(td(2*m)).ne.0.0d0
     &                         .or. ilast.eq.0) then
            ilast       = ilast + 1
            tv(1,ilast) = td(2*m-1)
            tv(2,ilast) = td(2*m)
          else
            done        = .true.
          endif
        enddo ! m
      end do ! while

      close(unit = lunit, status = 'delete')
      ior = iosav

      prop2p = 0.0d0

      end function prop2p
