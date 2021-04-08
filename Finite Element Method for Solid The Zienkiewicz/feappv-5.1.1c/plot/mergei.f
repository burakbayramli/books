!$Id:$
      subroutine mergei ( dir, nip, ip, numa, ipc )

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
!          nip        - Dimension of ip array
!          ip(nip,i)  - list of pointer  values
!          numa       - number of items in list

!      Scratch:
!          ipc(nip,i)  - working array for sort
!                        ic must contain nip*numa locations

!      Output:
!          ip(nip,i)  - sorted list of pointer values
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: a,au,b,bu,c,dir,i,ii,inc,j,nip,numa,step,npass
      integer       :: ip(nip,*),ipc(nip,*)

      save

      inc = 1
      do ii = 1,numa
        do j = 1,nip
          ipc(j,ii) = 0
        end do ! j
      end do

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
          if(ip(1,a).le.ip(1,b)) then
            do j = 1,nip
              ipc(j,c) = ip(j,a)
            end do ! j
            a     = a + 1
          else
            do j = 1,nip
              ipc(j,c) = ip(j,b)
            end do ! j
            b     = b + 1
          endif

        else

!         Decreasing sort
          if(ip(1,a).ge.ip(1,b)) then
            do j = 1,nip
              ipc(j,c) = ip(j,a)
            end do ! j
            a     = a + 1
          else
            do j = 1,nip
              ipc(j,c) = ip(j,b)
            end do ! j
            b     = b + 1
          endif
        endif

!       Copy remaining list when first list is finished
        if(a.gt.au) then
          do i = b, bu
            c     = c + 1
            do j = 1,nip
              ipc(j,c) = ip(j,i)
            end do ! j
          end do
        elseif(b.gt.bu) then
          do i = a, au
            c     = c + 1
            do j = 1,nip
              ipc(j,c) = ip(j,i)
            end do ! j
          end do

!       Increment and repeat for next list items
        else
          c = c + 1
          go to 110
        endif

!       Lists have been sorted using ic, copy back to original list
        b = ii - 1
        do i = ii,bu
          do j = 1,nip
            ip(j,i) = ipc(j,i-b)
          end do ! j
        end do
      end do

!     Increment step size for next pass
      inc   = step
      npass = npass + 1

      if(inc.lt.numa) go to 100

      end subroutine mergei
