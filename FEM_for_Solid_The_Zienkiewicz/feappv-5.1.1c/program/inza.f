!$Id:$
      integer function inza(n1, n2, ir, k, n)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Locate term in sparse matrix

!      Inputs:
!         n1     - First location to search in ir
!         n2     - Last location to search in ir
!         ir(*)  - Array with locations in row
!         k      - Column number to find
!         n      - Equation number

!      Outputs:
!         inza   - Location of term
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'

      integer      :: n1,n2, i,k,n, ir(*)

      save

!     Find term for assembly.

      do i = n1, n2
        if(ir(i).eq. k) then
          inza = i
          return
        endif
      end do ! i

!     ERROR if loop exits.

      inza = 0  ! Error
      write(  *,2000) n,k,n1,n2,(ir(i),i=n1,n2)
      write(iow,2000) n,k,n1,n2,(ir(i),i=n1,n2)
      call plstop(.true.)

!     Format

2000  format(/5x,'INZA: Eq. No. =',i8,' Col. No. =',i8/
     &        5x,'     Col. top =',i8,' Col. Bot =',i8/
     &        5x,'     Column entries'/ (5x,8i8))

      end function inza
