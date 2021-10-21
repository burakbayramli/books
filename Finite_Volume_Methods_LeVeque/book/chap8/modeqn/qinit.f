c
c
c =========================================================
       subroutine qinit(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux)
c =========================================================
c
c     # Set initial conditions for q.
c
c
      implicit double precision (a-h,o-z)
      dimension q(1-mbc:maxmx+mbc, meqn)
      dimension aux(1-mbc:maxmx+mbc, *)
c
c
      do 150 i=1,mx
	 xcell = xlower + (i-0.5d0)*dx
c        # Heaviside
         q(i,1) = 0.d0
         if (xcell .lt. 0.d0) q(i,1) = 2.d0
  150    continue
c
      return
      end
