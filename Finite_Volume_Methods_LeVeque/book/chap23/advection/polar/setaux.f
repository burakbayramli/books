c     ============================================
      subroutine setaux(maxmx,maxmy,mbc,mx,my,xlower,ylower,dxc,dyc,
     &                  maux,aux)
c     ============================================
c
c     # set auxiliary arrays for advection on a curvilinear grid
c
c     # on input, (xc(i),yc(j)) gives uniformly spaced computational grid.
c     # on output, 
c     #   aux(i,j,1) is edge velocity at "left" boundary of grid point (i,j)
c     #   aux(i,j,2) is edge velocity at "bottom" boundary of grid point (i,j)
c     #   aux(i,j,3) = kappa  is ratio of cell area to dxc*dyc
c     
      implicit double precision (a-h,o-z)
      dimension aux(1-mbc:maxmx+mbc,1-mbc:maxmy+mbc, 3)
      dimension xccorn(5),yccorn(5),xpcorn(5),ypcorn(5)
c
c
      do 20 j=1-mbc,my+mbc
         do 20 i=1-mbc,mx+mbc
c
c           # computational points (xc,yc) are mapped to physical
c           # coordinates (xp,yp) by mapc2p:
c
c           # lower left corner:
	    xccorn(1) = xlower + (i-1)*dxc
	    yccorn(1) = ylower + (j-1)*dyc
	    call mapc2p(xccorn(1),yccorn(1),xpcorn(1),ypcorn(1))

c           # upper left corner:
	    xccorn(2) = xccorn(1)
	    yccorn(2) = yccorn(1) + dyc
	    call mapc2p(xccorn(2),yccorn(2),xpcorn(2),ypcorn(2))
c
c           # upper right corner:
	    xccorn(3) = xccorn(1) + dxc
	    yccorn(3) = yccorn(1) + dyc
	    call mapc2p(xccorn(3),yccorn(3),xpcorn(3),ypcorn(3))
c
c           # lower right corner:
	    xccorn(4) = xccorn(1) + dxc
	    yccorn(4) = yccorn(1)
	    call mapc2p(xccorn(4),yccorn(4),xpcorn(4),ypcorn(4))
c
c
c           # compute edge velocities by differencing stream function:
c
	    aux(i,j,1) = (stream(xpcorn(2),ypcorn(2))
     &			  - stream(xpcorn(1),ypcorn(1)))/ dyc
c
	    aux(i,j,2) = -(stream(xpcorn(4),ypcorn(4))
     &			  - stream(xpcorn(1),ypcorn(1)))/ dxc

c
c
c           # compute area of physical cell from four corners:

	    xpcorn(5) = xpcorn(1)
	    ypcorn(5) = ypcorn(1)
	    area = 0.d0
	    do ic=1,4
	      area = area + 0.5d0 * (ypcorn(ic)+ypcorn(ic+1)) *
     &               (xpcorn(ic+1)-xpcorn(ic))
	      enddo
	    aux(i,j,3) = area / (dxc*dyc)
c
   20       continue
c
       return

       end
