!$Id:$
      subroutine pgetd( aname, point, lengt, prec , flag )

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Retrieve an array from dictionary

!      Inputs:
!         aname    - Name of array to retrieve

!      Outputs:
!         point    - Pointer to array
!         lengt    - Length of array
!         prec     - Precision of array
!         flag     - Flag, true if array found
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'allotn.h'
      include   'allotd.h'
      include   'debugs.h'
      include   'iofile.h'
      include   'pointer.h'

      include   'p_point.h'

      character (len=5) :: dname
      character         :: aname*(*)

      logical       :: pcomp,flag
      integer       :: lengt, prec, i

      save

!     Search dictionary for name

      dname = aname

      do i = 1,ndict

!       Assign pointer, length, and precision

        if( pcomp(dname, dict(i), 5) ) then
          point =  np(dlist(i))
          lengt =  ipoint(i)
          prec  =  iprec(i)
          flag  = .true.
          return
        endif
      end do ! i

!     Output error message if not found

      if(debug) then
        write(  *,2000) dname(1:5)
        write(iow,2000) dname(1:5)
        call plstop(.true.)
      end if
      flag  = .false.
      point = 0
      lengt = 0
      prec  = 0

!     Format

2000  format(' *WARNING* Check for ',a5,
     &       ': Array not allocated for this problem.')

      end subroutine pgetd
