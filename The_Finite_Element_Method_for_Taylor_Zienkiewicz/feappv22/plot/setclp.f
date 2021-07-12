c$Id:$
      subroutine setclp(x,ndm,numnp)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Compute clip range from mesh data

c      Inputs:
c         x(ndm,*)  - Nodal coordinates of mesh
c         ndm       - Spatial dimension of region
c         numnp     - Number of nodes in mesh

c      Outputs:
c         none      - Output through common /plclip/
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'plclip.h'
      include  'pointer.h'
      include  'comblk.h'

      logical   flag
      integer   i,n,ndm,numnp
      integer   nty
      real*8    x(ndm,*)

      save

      flag = .true.
      nty  =  np(49) - 1
      do n = 1,numnp
        if(mr(nty+n).ge.0) then

c         Set defaults to an existing coordinate

          if( flag ) then
            do i = 1,ndm
              cmin(i) = x(i,n)
              cmax(i) = x(i,n)
            end do
            flag = .false.
          end if

c         Set min / max for other coordinates

          do i = 1,ndm
            cmin(i) = min( cmin(i), x(i,n))
            cmax(i) = max( cmax(i), x(i,n))
          end do
        end if
      end do

      end
