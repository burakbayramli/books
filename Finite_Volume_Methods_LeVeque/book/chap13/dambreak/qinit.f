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
      common /cqinit/ sloc,hl,ul,hr,ur
c
c
c      # data in left state:
       hul = hl*ul
c  
c      # data in right state:
       hur = hr*ur
c
       do 150 i=1,mx
	     xedge = xlower + (i-1)*dx
	     if (xedge .lt. sloc) then
		 q(i,1) = hl
		 q(i,2) = hul
		else 
		 q(i,1) = hr
		 q(i,2) = hur
		endif
  150        continue
c
      return
      end
