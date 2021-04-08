!$Id:$
      subroutine front(nfrnt,nd,n,numb,ntag)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute front width for profile optimization.

!      Inputs:
!         n         - Number of node to add or remore
!         numb      - Number of nodes on front
!         ntag      - Indicator to add or remove node from front

!      Outputs:
!         nfrnt(*)  - Current nodes on front
!         nd(*)     - Indicator on active nodes
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer      :: l, m, n,numb,ntag, nfrnt(*),nd(*)

      save

!     Subroutine to update nodes on front

      if(ntag.eq.1) then

!       Add node to front if new node

        do m=1,numb
          if(n.eq.nfrnt(m)) return
        end do ! m
        numb = numb + 1
        nfrnt(numb) = n
        nd(n) = -1
      else

!       Remove node from front

        do m=1,numb
          if(n.eq.nfrnt(m)) go to 220
        end do ! m

  220   numb = numb - 1
        if(m.le.numb) then

          do l=m,numb
            nfrnt(l) = nfrnt(l+1)
          end do ! l
        endif
      endif

      end subroutine front
