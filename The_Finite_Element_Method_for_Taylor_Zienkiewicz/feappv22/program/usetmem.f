c$Id:$
      logical function usetmem(ulist,names,
     &                        num,name,length,precis)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Define, delete, or resize a dictionary entry.
c               Pointer defined for integer (single) and real
c               (double precision arrays.

c      Inputs:
c         ulist      - Number of entries in user list
c         names(*)   - Admissible names for user arrays
c         num        - Entry number for array
c         name       - Name of array
c         length     - Length of array defined: =0 for delete
c         precis     - Precision of array: 1 = integers; 2 = reals

c      Outputs:
c         up(num)    - Pointer to first word of array in blank common
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'allotd.h'
      include  'allotn.h'
      include  'pointer.h'

      character names(*)*(*),name*(*)
      logical   setmem
      integer   ulist,num,length,precis,i,tlist

      save

c     Merge lists

      if(num.eq.-llist) then
        do i = 1,ulist
          up(i)          = 0
          nlist(llist+i) = names(i)
        end do
        tlist = ulist + llist

c     Allocate or deallocate an array

      else
        do i = 1,ulist
          np(llist+i) = up(i)
        end do
        usetmem = setmem(tlist,ilist,nlist,num+llist,name,length,precis)
        do i = 1,ulist
          up(i) = np(llist+i)
        end do
      endif

      end
