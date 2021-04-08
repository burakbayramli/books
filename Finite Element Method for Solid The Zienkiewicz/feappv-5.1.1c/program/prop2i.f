!$Id:$
      function prop2i(lunit,l,ilast)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Proportional load table type 2 input/computation

!                prop = vm(i)+(vm(i+1)-vm(i))*(t-tm(i))/(tm(i+1)-tm(i)
!                       tm(i) < t < tm(i+1)
!      Inputs:
!         l         - Number of data input pairs to input/record
!                     Compute proportional load if zero.

!      Outputs:
!         ilast     - Number of entries in table
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'comfil.h'
      include  'iodata.h'
      include  'iofile.h'
      include  'print.h'

      character (len=128) :: fnamr

      logical       :: errck, pinput, done
      integer       :: l, m, ilast, lunit
      real (kind=8) :: prop2i, td(16)

      save

!     Input table of proportional loads

      ilast = 0

!     Find an unused unit

      done = .true.
      lunit = 16
      do while(done)
        lunit = lunit + 1
        inquire(unit = lunit, opened = done)
      end do ! while

!     Start read from temporary file 'finp' with 'T' prefix

      fnamr      = finp
      fnamr(1:1) = 'T'
      open(unit = lunit, file = fnamr)
      rewind lunit
      if(ior.lt.0) write(*,2001)
      if(prt) then
        write(iow,2002)
      endif
      done = .false.
      do while(.not.done)
102     if(ior.lt.0) then
          write(*,2000)
        endif
        errck = pinput(td,2*l)
        write(lunit,'(a)') record
        if(errck) go to 102
        do m = 1,l
          if(abs(td(2*m-1))+abs(td(2*m)).ne.0.0d0
     &                         .or. ilast.eq.0) then
            ilast       = ilast + 1
            if(prt) then
              write(iow,2004) ilast,td(2*m-1),td(2*m)
            endif
          else
            done        = .true.
          endif
        enddo ! m
      end do ! while

      prop2i = 0.0d0

!     Formats

2000  format('   > ',$)

2001  format(' Input: time and value (terminate with blank record)')

2002  format( '  Linear Interpolation Table'/
     &        '   No.    Time        Value')

2004  format(i5,1p,2e14.5)

      end function prop2i
