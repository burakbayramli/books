
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
       common /comic/ hl,hr,hul,hur

c
       do 20 i=1,mx
	  xc = xlower + (i-0.5d0)*dx
	  xclow = xlower + (i-1.0d0)*dx
          do 20 j=1,my
	     yc = ylower + (j-0.5d0)*dy
	     yclow = ylower + (j-1.0d0)*dy

c            # map the center of this computational cell to physical
c            # coordinates before evaluating the initial value funcion:
	     call mapc2p(xc,yc,xp,yp)

c	     q(i,j,1) = 1.d0  + 
c    &		3.d0*dexp(-50.d0*((xp-1.0d0)**2 + (yp-1.0d0)**2))
c    &		dexp(-100.d0*((xp-0.8d0)**2 + (yp-1.0d0)**2))

             call cellave(xclow,yclow,dx,dy,win)
	     q(i,j,1) = hl*win + hr*(1.d0-win)
             q(i,j,2) = hul*win + hur*(1.d0-win)
             q(i,j,3) = 0.d0

  20         continue
       return
       end
