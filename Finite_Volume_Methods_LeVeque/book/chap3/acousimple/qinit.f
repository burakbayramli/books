c
c
c =========================================================
       subroutine qinit(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux)
c =========================================================
c
c     # Set initial conditions for q.
c     # Pulse in pressure, zero velocity
c
c
      implicit double precision (a-h,o-z)
      dimension q(1-mbc:maxmx+mbc, meqn)
      dimension aux(1-mbc:maxmx+mbc, *)
      common /cqinit/ beta
c
c
      do 150 i=1,mx
	 xcell = xlower + (i-0.5d0)*dx
	 q(i,1) = 0.5d0*dexp(-80.d0*xcell**2)
	 if (dabs(xcell+0.2d0) .lt. 0.1d0)  then
	    q(i,1) = q(i,1) + 0.5d0
	    endif
	 q(i,2) = 0.d0
  150    continue
c
      return
      end
