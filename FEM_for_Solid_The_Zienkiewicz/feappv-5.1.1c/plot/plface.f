!$Id:$
      subroutine plface(ix,ip,x,ndm,nen1,numnp,numel,iln,ct)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Driver routine to display visible faces of elements
!               in perspective views.

!      Inputs:
!         ix(nen1,*)- Nodal connection list
!         ip(*)     - Sorted order for symmetry plots
!         x(ndm,*)  - Nodal coordinates for plot
!         nen1      - Dimension of ix array
!         numnp     - Number of nodes
!         numel     - Number of elements/faces
!         iln(*)    - Line type data
!         ct        - Option to plot faces with negative normals

!      Outputs:
!         none      - Plot outputs to screen/file
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'pdata4.h'

      integer       :: n, ndm, nen1, numnp, numel, nface, iln(2)

      integer       :: ix(nen1,numel), ip(numel)
      real (kind=8) :: x(ndm,numnp),ct

      save

!     Plot faces which are visible

      nface = 0
      do n = 1,numel
        if(ix(nen1,n).gt.0) then
          ip(n) = n
          call pfacev(ix(1,n),x,ndm,iln,ct,ip(n),nface)
        else
          ip(n) = 0
        endif
      end do

!     Pack ip array

      nface = 0
      do n = 1,numel
        if(ip(n).gt.0 .and. ix(nen1,n).gt.0) then
          nface = nface + 1
          ip(nface) = ip(n)
          if(n.gt.nface) then
            ip(n) = 0
          endif
        endif
      end do
      nfac = nface

!     Set line type to original

      call plline(iln)

      end subroutine plface
