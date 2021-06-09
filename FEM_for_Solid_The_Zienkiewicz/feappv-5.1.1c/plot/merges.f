!$Id:$
      subroutine merges ( dir, za, nip, ip, numa, ic )

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Merge sort routine

!      Inputs:
!          dir        - direction of sort
!                        > 0 for increasing sort;
!                        = 0 for increasing sort;
!                        < 0 for decreasing sort.
!          za(i)      - list of unsorted values
!          nip        - Dimension of ip array
!          ip(nip,i)  - list of pointer  values
!          numa       - number of items in list

!      Scratch:
!          ic(i)      - working array for sort
!                         ic must contain numa locations

!      Output:
!          za(i)      - unsorted list of values
!          ip(nip,i)  - sorted list of pointer values
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: a,au,b,bu,c,dir,i,ii,inc,nip,numa,step,npass
      integer       :: ic(*),ip(nip,*)
      real (kind=4) :: za(*)

      save

      inc = 1
      do ii = 1,numa
        ic(ii) = 0
      end do ! ii

!     Perform sort on list pairs in increments of step
      npass = 0
 100  step = min(2*inc,max(1,numa))
      do ii = 1,numa,step

!       Set values for pointers - limit to entries in list
        a    = ii
        au   = min(a  - 1 + inc,numa)
        b    = min(au + 1,      numa)
        bu   = min(b  - 1 + inc,numa)
        c = 1

!       Perform merge of two lists
 110    if(dir.ge.0) then

!         Increasing sort
          if(za(ip(1,a)).le.za(ip(1,b))) then
            ic(c) = ip(1,a)
            a     = a + 1
          else
            ic(c) = ip(1,b)
            b     = b + 1
          endif

        else

!         Decreasing sort
          if(za(ip(1,a)).ge.za(ip(1,b))) then
            ic(c) = ip(1,a)
            a     = a + 1
          else
            ic(c) = ip(1,b)
            b     = b + 1
          endif
        endif

!       Copy remaining list when first list is finished
        if(a.gt.au) then
          do i = b, bu
            c     = c + 1
            ic(c) = ip(1,i)
          end do ! i
        elseif(b.gt.bu) then
          do i = a, au
            c     = c + 1
            ic(c) = ip(1,i)
          end do ! i

!       Increment and repeat for next list items
        else
          c = c + 1
          go to 110
        endif

!       Lists have been sorted into ic, copy back to original list
        b = ii - 1
        do i = ii,bu
          ip(1,i) = ic(i-b)
        end do ! i
      end do ! ii

!     Increment step size for next pass
      inc  = step
      npass = npass + 1

      if(inc.lt.numa) go to 100

      end subroutine merges
