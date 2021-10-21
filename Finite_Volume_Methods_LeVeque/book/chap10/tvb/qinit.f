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
      common /comlim/ phiM,phiMdx2
c
c
c     # set phiMdx2, needed in limiter.  
c     # phiM set in setprob
      phiMdx2 = phiM * dx**2
c
      pi2 = 8.d0*datan(1.d0)  !# = 2 * pi
      do 150 i=1,mx
	 xcell = xlower + (i-0.5d0)*dx
c        # wave packet
         q(i,1) = dexp(-beta*(xcell-0.5d0)**2) * dsin(freq*xcell)
  150    continue
c
      return
      end
