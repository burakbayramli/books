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
      common /comic/ beta,freq
c
c
      pi2 = 8.d0*datan(1.d0)  !# = 2 * pi
      do 150 i=1,mx
	 xcell = xlower + (i-0.5d0)*dx
c        # wave packet
         q(i,1) = dexp(-beta*(xcell-0.5d0)**2) 
     &	         * dcos(freq*(xcell-0.5d0))
c        # square pulse
         if (xcell.gt.1.0d0 .and. xcell.lt.1.5d0) then
	     q(i,1) = q(i,1) + 1.d0
	     endif
  150    continue
c
      return
      end
