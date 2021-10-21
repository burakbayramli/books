c     ============================================
      subroutine setaux(maxmx,maxmy,mbc,mx,my,xlower,ylower,dx,dy,
     &                  maux,aux)
c     ============================================
c
c     # set auxiliary arrays 

c     #   aux(i,j,1) is edge velocity at "left" boundary of grid point (i,j)
c     #   aux(i,j,2) is edge velocity at "bottom" boundary of grid point (i,j)
c
c     
      implicit double precision (a-h,o-z)
      dimension aux(1-mbc:maxmx+mbc,1-mbc:maxmy+mbc, maux)

      do 20 i=1-mbc,mx+mbc
         do 20 j=1-mbc,my+mbc

c           # coordinates of lower left corner of grid cell:
	    xll = xlower + (i-1)*dx
	    yll = ylower + (j-1)*dy

c           # difference stream function psi to get normal velocities:
	    aux(i,j,1) =  (psi(xll, yll+dy) - psi(xll,yll)) / dy
	    aux(i,j,2) = -(psi(xll+dx, yll) - psi(xll,yll)) / dx
   20       continue

c
       return
       end
