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
      
      common /comic/ ur, ul
c
c
      x0 = 0.0d0
c
c     # set Riemann initial values
      do 150 i=1,mx
	 xcell = xlower + (i-0.5d0)*dx
         if(xcell.lt.x0) then
            q(i,1) = ul
         else
            q(i,1)= ur
         endif
  150    continue

c
      return
      end
