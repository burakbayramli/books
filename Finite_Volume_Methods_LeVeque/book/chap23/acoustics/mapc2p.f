c
c     =====================================================
      subroutine mapc2p(xc,yc,xp,yp)
c     =====================================================
c
c     # on input,  (xc,yc) is a computational grid point
c     # on output, (xp,yp) is corresponding point in physical space
c
      implicit double precision (a-h,o-z)
c
c     # radial coordinates, xc = r,  yc = theta
c
c     xp = xc + (yc+1.d0)/2.d0

      xp = xc + (dabs(yc+.2d0)+ .8d0)/2d0
      yp = yc

c
      return
      end
