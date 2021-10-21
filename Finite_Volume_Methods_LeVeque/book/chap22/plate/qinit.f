
c
c
c
c     =====================================================
       subroutine qinit(maxmx,maxmy,meqn,mbc,mx,my,xlower,ylower,
     &                   dx,dy,q,maux,aux)
c     =====================================================
c
c     # Set initial conditions for q.
c
       implicit double precision (a-h,o-z)
       dimension q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
       dimension aux(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, maux)
       common /comaux/ rho1,amu1,alam1,rho2,amu2,alam2
c
       do 20 i=1,mx
	  xi = xlower + (i-0.5d0)*dx
          do 20 j=1,my
	     yj = ylower + (j-0.5d0)*dy
	     q(i,j,1) = 0.d0
	     q(i,j,2) = 0.d0
	     q(i,j,3) = 0.d0
	     q(i,j,4) = 0.d0
	     q(i,j,5) = 0.d0
  20         continue
       return
       end
