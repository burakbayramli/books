c
c
c =========================================================
       subroutine qinit(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux)
c =========================================================
c
c     # Set initial conditions for q.
c
      implicit double precision (a-h,o-z)
      dimension q(1-mbc:maxmx+mbc, meqn)
      dimension aux(1-mbc:maxmx+mbc, *)
      common /comrp/ grav
c
c  
c      # data in middle state:
       h2 = 2.d0
       u2 = 0.d0
       hu2 = h2*u2
c
c      # data in left state:
       h1 = 4.d0
       u1 = dsqrt(0.5d0*grav*(h2/h1 - h1/h2) * (h2-h1))
       hu1 = h1*u1
c  
c      # data in right state:
       h3 = 0.8d0
       u3 = -dsqrt(0.5d0*grav*(h2/h3 - h3/h2) * (h2-h3))
       hu3 = h3*u3
c
c
       s12 = (hu2-hu1) / (h2-h1)
       s23 = (hu2-hu3) / (h2-h3)
       write(6,*) 'shock speeds:', s12, s23

       do 150 i=1,mx
	     xedge = xlower + (i-1)*dx
	     if (xedge .lt. -4.0d0) then
		 q(i,1) = h1
		 q(i,2) = hu1
		else if (xedge .lt. -2.5d0) then
		 q(i,1) = h2
		 q(i,2) = hu2
		else 
		 q(i,1) = h3
		 q(i,2) = hu3
		endif
  150        continue
c
      return
      end
