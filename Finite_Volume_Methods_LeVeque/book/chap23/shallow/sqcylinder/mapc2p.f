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
c     # squared off radial coordinates, xc = fraction of radius,  yc = theta
c
      r0 = 1.d0
      r1 = 5.d0
      pi = 4.d0*datan(1.d0)

      yc1 = dmin1(yc, dabs(pi/2-yc))
      yc1 = dmin1(yc1, dabs(pi-yc))
      yc1 = dmin1(yc1, dabs(3*pi/2-yc))
      yc1 = dmin1(yc1, dabs(2*pi-yc))
      r = r0 + (r1*dsqrt(1.d0 + dtan(yc1)**2) - r0) * xc
      xp = r * dcos(yc)
      yp = r * dsin(yc)

c
      return
      end
