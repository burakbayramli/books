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
	 if (xcell .gt. 0.d0) then
	     q(i,1) = 1.d0
	   else
	     q(i,1) = 2.d0
	   endif
  150    continue
c
      return
      end
