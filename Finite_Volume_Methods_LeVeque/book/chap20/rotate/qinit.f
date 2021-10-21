
c
c
c
c     =====================================================
       subroutine qinit(maxmx,maxmy,meqn,mbc,mx,my,xlower,ylower,
     &                   dx,dy,q)
c     =====================================================
c
c     # Set initial conditions for q.
c     # Sample scalar equation with data that is piecewise constant with
c     # q = 1.0  if  0.2 < x < 0.6   and   0.2 < y < 0.6
c     #     0.0  otherwise
c
       implicit double precision (a-h,o-z)
       dimension q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
c
       do 20 i=1,mx
	  xi = xlower + (i-0.5d0)*dx
          do 20 j=1,my
	     yj = ylower + (j-0.5d0)*dy
 	     if (xi.lt.0.6d0 .and. xi.gt.0.1d0 .and. yj.gt.-0.25d0 .and.
     &		 yj.lt.0.25d0) then
		     q(i,j,1) = 1.d0
		   else
		     q(i,j,1) = 0.d0
		   endif
	     r = dsqrt((xi+0.45d0)**2 + yj**2)
	     if (r .lt. 0.35d0) then
	         q(i,j,1) = 1.d0 - r/0.35d0
		 endif

  20         continue
       return
       end
