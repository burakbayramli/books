c$Id:$
      subroutine plface(ix,ip,x,ndm,nen1,numnp,numel,iln,ct)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Driver routine to display visible faces of elements
c               in perspective views.

c      Inputs:
c         ix(nen1,*)- Nodal connection list
c         ip(*)   - Sorted order for symmetry plots
c         x(ndm,*)  - Nodal coordinates for plot
c         nen1      - Dimension of ix array
c         numnp     - Number of nodes
c         numel     - Number of elements/faces
c         iln(*)    - Line type data
c         ct        - Option to plot faces with negative normals

c      Outputs:
c         none      - Plot outputs to screen/file
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'pdata4.h'

      integer   n, ndm, nen1, numnp, numel, nface, iln(2)

      integer   ix(nen1,numel), ip(numel)
      real*8    x(ndm,numnp),ct

      save

c     Plot faces which are visible

      nface = 0
      do n = 1,numel
        if(ix(nen1,n).gt.0) then
          ip(n) = n
          call pfacev(ix(1,n),x,ndm,iln,ct,ip(n),nface)
        else
          ip(n) = 0
        endif
      end do

c     Pack ip array

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

c     Set line type to original

      call plline(iln)

      end
