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
      common /comtraf/ u1,u2,rho1,rho2
c
c
      do 150 i=1,mx
	 xcell = xlower + (i-0.5d0)*dx
         if (xcell.lt.0.0d0) then
	     q(i,1) = rho1
	    else
	     q(i,1) = rho2
	    endif
  150    continue
c
      return
      end
