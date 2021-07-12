c$Id: pltelm.f,v 1.1 2000/08/24 20:49:59 rlt Exp $
      subroutine pltelm(x,ie,ix,scale,nie,ndm,nen1,n1,n2)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--+---------+---------+---------+---------+---------+---------+-]
c      Purpose: Place element numbers on plots of mesh

c      Inputs:
c         x(ndm,*)  - Nodal coordinates of mesh
c         ie(nie,*) - Assembly data for material sets
c         ix(nen1,*)- Element nodal connections
c         scale     - Plot scale factor
c         nie       - Dimension of ie array
c         ndm       - Dimension of x array
c         nen1      - Dimension of ix array
c         n1        - First element number to display
c         n2        - Last element number to display

c      Outputs:
c         none      - Plot outputs to screen/file
c-----[--+---------+---------+---------+---------+---------+---------+-]

      implicit  none

      include  'pbody.h'
      include  'pdata4.h'
      include  'plflag.h'

      logical   zoom
      integer   nie,ndm,nen1, i,j,n,ii,jj,nn,ma,nd, n1,n2, iplt(50)
      real*8    scale,dx1

      integer   ie(nie,*),ix(nen1,*)
      real*8    x(ndm,*),xx(3)

      save

c     Write element labels

      dx1 = .005d0/scale
      nd = 2
      do n = n1,n2
        ma = ix(nen1,n)
        if(ix(nen1-1,n).ge.0 .and. maplt.eq.0 .or. ma.eq.maplt) then
          xx(1) = 0.0d0
          xx(2) = 0.0d0
          xx(3) = 0.0d0
          jj = 0
          call pltord(ix(1,n),ie(nie-1,ma), nn,iplt)
          nn = max(1,nn-1)
          do i = 1,nn
            j  = iplt(i)
            ii = ix(j,n)
            if(ii.gt.0) then
              jj = jj + 1
              xx(1) = xx(1) + x(1,ii)
              xx(2) = xx(2) + x(2,ii)
              if(ndm.ge.3) xx(3) = xx(3) + x(3,ii)
            endif
          end do
          if(jj.gt.0) then
            xx(1) = xx(1)/jj
            xx(2) = xx(2)/jj
            xx(3) = xx(3)/jj
            if(zoom(xx(1),ndm)) then
              call plotl(xx(1)-dx1*nd,xx(2)-dx1,xx(3),3)
              if(clip) call plabl(n)
            endif
          endif
        endif
      end do

      end
