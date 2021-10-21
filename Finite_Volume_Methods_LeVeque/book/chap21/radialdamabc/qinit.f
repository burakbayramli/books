
c
c
c
c     =====================================================
       subroutine qinit(maxmx,maxmy,meqn,mbc,mx,my,xlower,ylower,
     &                   dx,dy,q,maux,aux)
c     =====================================================
c
c     # Set initial conditions for q.
c     # Shallow water with radial dam break problem, h = hin inside
c     # circle specified in fdisc.f
c
       implicit double precision (a-h,o-z)
       dimension q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
       common /comic/ hin,hout
c

       do 20 i=1,mx
          xlow = xlower + (i-1.d0)*dx
          do 20 j=1,my
             ylow = ylower + (j-1.d0)*dy
	     call cellave(xlow,ylow,dx,dy,win)
	     q(i,j,1) = hin*win + hout*(1.d0-win)
	     q(i,j,2) = 0.d0
	     q(i,j,3) = 0.d0
  20         continue
       return
       end
