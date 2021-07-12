c$Id:$
      subroutine shp1d(s,xl,shp,ndm,nel,xjac)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c     Purpose: Compute shape functions, natural derivatives, and
c              jacobian for 3-D beam at natural coordinate s.
c              Linear (2 nodes) or quadratic (3 nodes) element.

c     Inputs:
c       s         : natural coordinate
c       xl(3,nel) : nodal global coordinates
c       ndm       : coordinate dimension of mesh
c       nel       : number of nodes of element

c     Outputs:
c       shp(2,nel): shape functions and derivatives at s
c                   shp(1,1 to nel): derivatives of shape functions
c                   shp(2,1 to nel): shape functions
c       xjac      : jacobian at s
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'iofile.h'

      integer   ndm,nel,i
      real*8    s,xjac,h,h2
      real*8    xl(ndm,nel),shp(2,nel),xx(3)

      save

c     Computation of length of element

      do i = 1,3
        xx(i) = 0.0d0
      end do
      do i = 1,ndm
        xx(i) = xl(i,nel) - xl(i,1)
      end do
      h  = sqrt(xx(1)**2 + xx(2)**2 + xx(3)**2)
      if(h.eq.0.d0) then
         write(iow,3000) h
         call plstop()
      endif

c     Linear element

      if(nel.eq.2) then

        shp(2,1) = (1.d0 - s)*0.5d0
        shp(2,2) = (1.d0 + s)*0.5d0
        xjac     =  h*0.5d0
        shp(1,1) = -1.d0/h
        shp(1,2) =  1.d0/h

c     Quadratic element

      elseif(nel.eq.3) then

        do i = 1,ndm
          xx(i) = xl(i,nel) - xl(i,nel-1)
        end do
        h2 = sqrt(xx(1)**2 + xx(2)**2 + xx(3)**2)
        if(h2.eq.0.d0) then
           write(iow,3001) h2
           call plstop()
        endif
        shp(2,1) =  s*(s - 1.d0)*0.5d0
        shp(2,2) =  1.d0 - s*s
        shp(2,3) =  s*(s + 1.d0)*0.5d0
        xjac     =  h*0.5d0 + s*(2.d0*h2-h)
        shp(1,1) = (s - 0.5d0)/xjac
        shp(1,2) = -2.d0*s/xjac
        shp(1,3) = (s + 0.5d0)/xjac
      endif

3000  format(/5x,'Error in SHP1D: h =',d12.5,/,5x,
     &'linear element with zero length')

3001  format(/5x,'Error in SHP1D: h2=',d12.5,/,5x,
     &'quadratic element with zero length')

      end
