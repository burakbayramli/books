c$Id:$
      subroutine cktets ( n, ix, xl, ndm, nel, shp )

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Check tetrahedra for valid data specifications.

c      Inputs:
c         n         - Element number being checked
c         ix(*)     - Number of nodes connected to element
c         xl(ndm,*) - Array of element nodal coordinates
c         ndm       - Spatial dimension of mesh
c         nel       - Number of nodes on this element

c      Outputs:
c         None

c      Scratch:
c         shp(*)    - Shape function array storage
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none
      include  'pointer.h'
      include  'comblk.h'

      include  'iofile.h'

      integer   ndm, nel, i, l, n, ineg
      integer   ndty
      real*8    detj

      integer   ic(22)
      integer   ix(*)
      real*8    rst(3,11), xl(ndm,*), shp(*)

      save

      data      rst/ 1.d0, 0.d0, 0.d0,   0.d0, 1.d0, 0.d0,
     &               0.d0, 0.d0, 1.d0,   0.d0, 0.d0, 0.d0,
     &               .5d0, .5d0, .0d0,   .0d0, .5d0, .5d0,
     &               .0d0, .0d0, .5d0,   .5d0, .0d0, .0d0,
     &               .0d0, .5d0, .0d0,   .5d0, .0d0, .5d0,
     &               .25d0,.25d0,.25d0/

c     Check element for input errors

      ndty = np(49) - 1
      ineg = 0
      do l = 1,nel
        if(ix(l).gt.0) then
          if(mr(ndty+ix(l)).lt.0) then
            ic(ineg+1) = l
            ic(ineg+2) = abs(ix(l))
            ineg = ineg + 2
          endif
        endif
      end do

c     Node numbering errors

      if(ineg.gt.0) then
        write(iow,2000) n,(ic(i),i=1,ineg)
        if(ior.lt.0) write(*,2000) n,(ic(i),i=1,ineg)

c     Compute jacobian at each corner of element

      else
        do l = 1,nel
          call tjac3d ( rst(1,l) , xl, ndm, nel, shp, detj )
          if(detj.le.0.0d0) then
            ic(ineg+1) = l
            ic(ineg+2) = abs(ix(l))
            ineg = ineg + 2
          endif
        end do
        if(ineg.gt.0) then
          write(iow,2001) n,(ic(i),i=1,ineg)
          if(ior.lt.0) write(*,2001) n,(ic(i),i=1,ineg)
        endif
      endif

2000  format(' >Element',i4,' coordinates not input for nodes:'/
     &      ('                Local =',i3,' Global =',i4))

2001  format(' >Element',i4,' has negative jacobian at nodes:'/
     &      ('                Local =',i3,' Global =',i4))

      end
