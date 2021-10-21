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
      common /comic/ beta
c
c
      do 150 i=1,mx
	 xcell = xlower + (i-0.5d0)*dx
	 q(i,1) = dexp(-beta * (xcell-0.3d0)**2)
	 if (xcell .lt. 0.d0) then
	     q(i,1) = 0.25d0
	   else
	     q(i,1) = 1.0d0
	   endif
  150    continue
c
      return
      end
