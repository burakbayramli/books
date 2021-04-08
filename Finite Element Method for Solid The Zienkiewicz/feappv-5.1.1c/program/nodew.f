!$Id:$
      subroutine nodew(ndw,msum,ix,nnid,nen,nen1,
     &                 numnp,numel,nstart)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute node and element weights for profile
!               minimizations

!      Inputs:
!         ix(nen1,*)     - Element nodal connection list
!         nnid(numnp)    - Number active dof at nodes
!         nen            - Maximum number nodes/element
!         nen1           - Dimension of ix  array
!         numnp          - Number of nodes in mesh
!         numel          - Number of elements in mesh

!      Outputs:
!         ndw(*)         - Node weights
!         msum(*)        - Element weights
!         nstart
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: k, l, m,minw, n,nel,nen,nen1
      integer       :: numnp,numel,nstart, nsum
      integer       :: ndw(numnp),msum(numel),ix(nen1,numel)
      integer       :: nnid(numnp)

      save

!     Evaluation of node weights

      do n = 1,numnp
        ndw(n) = 1
      end do ! n
      minw = 1

!     Loop five times to evaluate node weights

      do k = 1,5

!       Normalize node weights

        do n = 1,numnp
          ndw(n) = ndw(n) / minw
        end do ! n

!       Compute element weights

        do m = 1,numel
          if(msum(m).gt.0.and.ix(nen1-1,m).ge.0) then
            nsum = 0
            nel  = 0
            do l = 1,nen
              n = abs(ix(l,m))
              if(n.gt.0) then
                if(nnid(n).gt.0) then
                  nel  = nel + 1
                  nsum = nsum + ndw(n)
                endif
              endif
            end do ! l
            nsum     = min(nsum,numnp)
            msum(m)  = nsum / max(1,nel)
          endif
        end do ! m

!       Compute node weights

        minw = 0
        do n = 1,numnp
          ndw(n) = 0
        end do ! n
        do m = 1,numel
          if (msum(m).gt.0.and.ix(nen1-1,m).ge.0) then
            nsum = 0
            do l = 1,nen
              n = abs(ix(l,m))
              if(n.gt.0) then
                if(nnid(n).gt.0) then
                  ndw(n) = ndw(n) + msum(m)
                  minw   = max(minw,ndw(n))
                endif
              endif
            end do ! l
          endif
        end do ! m

!       Find minimum value

        minw = 32000000
        do n = 1,numnp
          if (ndw(n).gt.0 .and. ndw(n).le.minw) then
            minw   = ndw(n)
            nstart = n
          endif
        end do ! n
      end do ! k

      end subroutine nodew
