c$Id:$
      subroutine mergei ( dir, nip, ip, numa, ipc )

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Merge sort routine

c      Inputs:
c          dir        - direction of sort
c                        > 0 for increasing sort;
c                        = 0 for increasing sort;
c                        < 0 for decreasing sort.
c          nip        - Dimension of ip array
c          ip(nip,i)  - list of pointer  values
c          numa       - number of items in list

c      Scratch:
c          ipc(nip,i)  - working array for sort
c                        ic must contain nip*numa locations

c      Output:
c          ip(nip,i)  - sorted list of pointer values

c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer   a,au,b,bu,c,dir,i,ii,inc,j,nip,numa,step,pass

      integer   ip(nip,*),ipc(nip,*)

      save

      inc = 1
      do ii = 1,numa
        do j = 1,nip
          ipc(j,ii) = 0
        end do ! j
      end do

c     Perform sort on list pairs in increments of step

      pass = 0
 100  step = min(2*inc,max(1,numa))
      do ii = 1,numa,step

c       Set values for pointers - limit to entries in list

        a    = ii
        au   = min(a  - 1 + inc,numa)
        b    = min(au + 1,      numa)
        bu   = min(b  - 1 + inc,numa)
        c = 1

c       Perform merge of two lists

 110    if(dir.ge.0) then

c         Increasing sort

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

c         Decreasing sort

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

c       Copy remaining list when first list is finished

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

c       Increment and repeat for next list items

        else
          c = c + 1
          go to 110
        endif

c       Lists have been sorted using ic, copy back to original list

        b = ii - 1
        do i = ii,bu
          do j = 1,nip
            ip(j,i) = ipc(j,i-b)
          end do ! j
        end do
      end do

c     Increment step size for next pass

      inc  = step
      pass = pass + 1

      if(inc.lt.numa) go to 100

      end
