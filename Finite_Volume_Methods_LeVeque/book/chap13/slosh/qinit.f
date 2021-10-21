
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
c
c
       do 20 i=1-mbc,mx+mbc
          do 20 j=1-mbc,my+mbc
	     q(i,j,1) = 1.d0
	     q(i,j,2) = 0.d0
	     q(i,j,3) = 0.d0
  20         continue
       return
       end
