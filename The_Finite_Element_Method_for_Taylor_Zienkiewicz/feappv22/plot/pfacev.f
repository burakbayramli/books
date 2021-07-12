c$Id:$
      subroutine pfacev(ix,x,ndm,iln,ct,ip,nface)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Plot visible exterior faces of 3-d objects

c      Inputs:
c         ix(4)      - List of face nodes
c         x(ndm,*)   - Nodal coordinates
c         iln(2)     - Line type data
c         ct         - Also plot back faces when > 0
c         ip(*)      - Sort data for hidden surface representations
c         nface      - Number of faces

c      Outputs:
c         none       - Plot outputs to screen/file
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'comblk.h'
      include  'pointer.h'

      logical   visbl
      integer   j, k, ndm, nface

      integer   ix(4), ip
      integer   iln(2),ilnd(2)
      real*8    x(ndm,*),xl(3,4),ct

      save

      data      ilnd(1)/1/

      ilnd(2) = iln(2)

c     Plot face

      do j = 1,4
        do k = 1,3
          xl(k,j) = x(k,ix(j))
        end do
      end do

      if(visbl(xl) .or. (ix(1).eq.ix(4) .and. ix(2).eq.ix(3))) then

        nface = nface + 1
        if(ct.ge.0.0d0) then
          call plline(iln)
          call plotl(xl(1,4),xl(2,4),xl(3,4),3)
          do j = 1,4
            call plotl(xl(1,j),xl(2,j),xl(3,j),2)
            mr(np(59)+ix(j)-1) = 1
          end do
        end if

      elseif(ct.gt.0.0d0) then

        ip = 0
        call plline(ilnd)
        call plotl(xl(1,4),xl(2,4),xl(3,4),3)
        do j = 1,4
          call plotl(xl(1,j),xl(2,j),xl(3,j),2)
        end do

      else

        ip = 0

      endif

      end
