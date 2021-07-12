c$Id:$
      subroutine perspz(x,ix, zn,ze,ip, nen1,nen,ndm,numnp,numel)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Sort perspective projection of coordinates by z-values

c      Inputs:
c         x (ndm,*) - Nodal coordinates
c         ix(nen1,*)- Element connection list
c         ndm       - 1st dimension of x
c         nen1      - 1st dimension of ix
c         nen       - Number nodes connected to elements
c         numel     - Total number of elements
c         numnp     - Total number of nodal points

c      Outputs:
c         zn(numnp) - Perspective z-coordinates(destroyed)
c         ze(numel) - Perspective z-coordinates
c         ip(*)     - Element order for z-coordinates
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'iofile.h'
      include  'ppers.h'
      include  'pdata4.h'
      include  'pointer.h'
      include  'comblk.h'

      integer   i,ii, n,nen1,nen,ndm,numnp,numel,nne, nty
      integer   ix(nen1,numel), ip(numel)
      real*8    t(3),x(ndm,numnp)
      real*8    zmax,zn(numnp),ze(numel)

      save

c     Loop over data points and find projection

      nty = np(49) - 1
      do n=1,numnp
        if(mr(nty+n).ge.0) then
          do i=1,3
            t(i) = x(i,n) - e(i)
          end do
          zn(n) = -xlbda(3,1)*t(1)-xlbda(3,2)*t(2)-xlbda(3,3)*t(3)
        else
          zn(n) = 0.0e0
        endif
      end do

c     Search visible faces for depth sort

      do n = 1,nfac
        nne = ip(n)
        if(nne.gt.0) then
          if(ix(nen1,nne).gt.0) then
            zmax  = 0.0e0
            do i = 1,nen
              ii = abs(ix(i,nne))
              if( ii.gt.0 ) then
                zmax = max(zmax,zn(ii))
              endif
            end do
            ze(nne)    = zmax
          endif
        endif
      end do

c     Sort element plot order array 'ip' to produce hidden surface

      call merges ( -1, ze, 1, ip, nfac, zn )

      end
