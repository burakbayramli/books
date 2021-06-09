!$Id:$
      subroutine pfacex(il,ix,ixf,nen,nen1,nf,n)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set up nodes for faces

!      Inputs:
!         il(*)   - Location of face nodes on element
!         ix(*)   - Node numbers on elements
!         nen     - Number of nodes on element
!         nen1    - Location of material set number on element
!         n       - Element number

!      Outputs:
!         ixf(*)  - Face nodes
!         nf      - Face number
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: nen,nen1,nf,n, j
      integer       :: il(4),ix(nen1)
      integer       :: ixf(7)

      save

!     Set face nodes

      do j = 1,min(4,nen)
        ixf(j) = ix(il(j))
      end do

!     Set region and material number

      ixf(5) = n
      ixf(6) = ix(nen1-1)
      ixf(7) = ix(nen1)
      nf = nf + 1

      end subroutine pfacex
