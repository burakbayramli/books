!$Id:$
      logical function usetmem(ulist,names,
     &                        num,name,length,precis)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Define, delete, or resize a dictionary entry.
!               Pointer defined for integer (single) and real
!               (double precision arrays.

!      Inputs:
!         ulist      - Number of entries in user list
!         names(*)   - Admissible names for user arrays
!         num        - Entry number for array
!         name       - Name of array
!         length     - Length of array defined: =0 for delete
!         precis     - Precision of array: 1 = integers; 2 = reals

!      Outputs:
!         up(num)    - Pointer to first word of array in blank common
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'allotd.h'
      include  'allotn.h'
      include  'pointer.h'

      character     :: names(*)*(*),name*(*)
      logical       :: setmem
      integer       :: ulist,num,length,precis,i,tlist

      save

!     Merge lists

      if(num.eq.-llist) then
        do i = 1,ulist
          up(i)          = 0
          nlist(llist+i) = names(i)
        end do ! i
        tlist   = ulist + llist
        usetmem = .true.

!     Allocate or deallocate an array

      else

        usetmem = setmem(tlist,ilist,nlist,num+llist,name,length,precis)

      endif

      end function usetmem
