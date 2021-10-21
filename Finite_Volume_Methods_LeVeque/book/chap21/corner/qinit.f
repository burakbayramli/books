
c
c
c
c     =====================================================
       subroutine qinit(maxmx,maxmy,meqn,mbc,mx,my,xlower,ylower,
     &                   dx,dy,q,maux,aux)
c     =====================================================
c
c     # Set initial conditions for q.
c     # Sample scalar equation with data that is piecewise constant with
c     # p = 1.0  if  0.1 < x < 0.6   and   0.1 < y < 0.6
c     #     0.0  otherwise
c
       implicit double precision (a-h,o-z)
       dimension q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
c
       do 20 i=1,mx
	  xi = xlower + (i-0.5d0)*dx
          do 20 j=1,my
	     yj = ylower + (j-0.5d0)*dy
	     if (xi .gt. -0.35d0 .and. xi.lt.-0.2d0) then
	         q(i,j,1) = 1.d0
		else
	         q(i,j,1) = 0.d0
		endif
	     q(i,j,2) = q(i,j,1)
	     q(i,j,3) = 0.d0
  20         continue
       return
       end
