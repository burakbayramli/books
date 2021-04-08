!$Id:$
      subroutine datric(iops,mops,jp,neq)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Symbolic decomposition of matrix stored in profile form

!      Inputs:
!         jp(*)  - Pointer to ends of rows/columns in A
!         neq    - Number of equations in A

!      Outputs:
!         iops   - Number of operations in excess of mega-op counter
!         mops   - Mega-operation counter
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'

      integer       :: i, id, ih, is, iops, mops, j, jd, jh, neq
      integer       :: jp(*)
      real (kind=4) :: etime, tt(2), tar

      save

!     Loop through the columns to count operations

      jd   = 1
      iops = 0
      mops = 0
      do j = 2,neq
        jh = jp(j) - jp(j-1) - 1
        if(jh.gt.0) then
          is = j - jh

!         Reduce each column

          do i = is,j-1
            id = jp(i)
            ih = min(id-jp(i-1),i-is+1)
            if(ih.gt.0) then
              iops = iops + ih + ih + 1
            endif
          end do
        endif

!       Reduce diagonal

        if(jh.ge.0) then
          iops = iops +3*jh + 2
        endif

!       Accumulate megaflops

!       if(iops.gt.1000000) then
        do while(iops.gt.1000000)
          iops = iops  - 1000000
          mops = mops + 1
        end do ! while
!       endif
      end do

!     Total number of operations

      tar = etime(tt)
      write(iow,2000) iops,mops,tt
      if(ior.lt.0) then
        write(  *,2000) iops,mops,tt
      end if

2000  format('   Number of operations =',i8,' plus ',i8,' Mega-ops'/
     &       '  Time: CPU = ',f12.2,' , System = ',f12.2)

      end subroutine datric
