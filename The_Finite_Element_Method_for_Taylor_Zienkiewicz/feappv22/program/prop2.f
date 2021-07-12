c$Id:$
      function prop2(t,l,tv,ilast,itime)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Proportional load table type 2 input/computation

c                    prop = vm(i)+(vm(i+1)-vm(i))*(t-tm(i))/(tm(i+1)-tm(i)
c                           tm(i) < t < tm(i+1)
c      Inputs:
c         t         - Time for lookup
c         l         - Number of data input pairs to input/record
c                     Compute proportional load if zero.

c      Outputs:
c         prop2     - Value of total proportional load type 2
c         tv(2,*)   - Table of times and values:
c                       tm(*) = tv(1,*); vm(*) = tv(2,*)
c         ilast     - Number of entries in table
c         itime     - Entry number defining interpolation segment
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'iofile.h'
      include  'print.h'

      logical   errck, pinput, done
      integer   l, m, ilast, itime
      real*8    t

      real*8    tv(2,*), td(16)

      real*8    prop2

      save

c     Input table of proportional loads

      if(l.gt.0) then

        ilast = 0
        itime = 1

        if(ior.lt.0) write(*,2001)
        if(prt) then
          write(iow,2002)
        endif
102     if(ior.lt.0) write(*,2003)
        errck = pinput(td,2*l)
        if(errck) go to 102
        done = .false.
        do m = 1,l
          if(abs(td(2*m-1))+abs(td(2*m)).ne.0.0d0
     &                         .or. ilast.eq.0) then
            ilast       = ilast + 1
            tv(1,ilast) = td(2*m-1)
            tv(2,ilast) = td(2*m)
            if(prt) then
              write(iow,2004) ilast,tv(1,ilast),tv(2,ilast)
            endif
          else
            done        = .true.
          endif
        enddo
        if(.not.done) go to 102

c     Compute value at time t by interpolating table

      else

c       Value of t less than first table value

        if(t.lt.tv(1,1)) then

          prop2 = tv(2,1)

c       Value of t greater than last table value

        elseif(t.ge.tv(1,ilast)) then

          prop2 = tv(2,ilast)

c       Interpolate table values

        else

          m = max(1,itime)
201       if(t.lt.tv(1,m)) then
            m = m - 1
            go to 201
          elseif(t.gt.tv(1,m+1)) then
            m = m + 1
            go to 201
          else
            prop2 =  tv(2,m)
     &            + (tv(2,m+1)-tv(2,m))*(t-tv(1,m))/(tv(1,m+1)-tv(1,m))
          endif
        endif
        itime = max(m,ilast-1)
      endif

c     Formats

2001  format(' Input: time and value (terminate with blank record)')

2002  format( '  Linear Interpolation Table'/
     1        '   No.    Time        Value')

2003  format('   >',$)

2004  format(i5,1p,2e14.5)

      end
