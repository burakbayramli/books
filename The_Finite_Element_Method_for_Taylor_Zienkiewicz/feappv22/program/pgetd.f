c$Id:$
      subroutine pgetd( name, point, lengt, prec , flag )

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Retrieve an array from dictionary

c      Inputs:
c         name     - Name of array to retrieve

c      Outputs:
c         point    - Pointer to array in blank common
c         lengt    - Length of array
c         prec     - Precision of array
c         flag     - Flag, true if array found
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'allotn.h'
      include  'allotd.h'
      include  'cdata.h'
      include  'debugs.h'
      include  'iofile.h'

      character name*(*),dname*5
      logical   pcomp,flag
      integer   point, lengt, prec, i, irp(2,2)

      save

      data      irp / 3*1, 2 /

c     Search dictionary for name

      dname = name

      do i = 1,ndict

c       Assign pointer, length, and precision

        if( pcomp(dname, dict(i), 5) ) then
          prec  =  irp(iprec(i),ipr)
          point = (ipoint(i) + prec - 1)/prec - ipr*(2 - iprec(i))
          lengt = (ipoint(i+1) - ipoint(i))/prec
          prec  =  iprec(i)
          flag  = .true.
          return
        endif
      end do

c     Output error message if not found

      if(debug) then
        if(ior.lt.0) write(*,2000) dname(1:5)
        write(iow,2000) dname(1:5)
c       if(ior.gt.0) call plstop()
      end if
      flag = .false.

c     Format

2000  format(' *WARNING* Check for ',a5,
     &       ': Array not allocated for this problem.')

      end
