c     ============================================
      subroutine setaux(maxmx,mbc,mx,xlower,dx,maux,aux)
c     ============================================
c
c     # set auxiliary arrays 
c     # for traffic flow with variable speed limit umax stored in aux(i,1)
c
c     
      implicit double precision (a-h,o-z)
      dimension aux(1-mbc:maxmx+mbc, 1)
      common /cout/ umax(-1:20002)
c
      pi = 4.d0*datan(1.d0)
      do 150 i=1-mbc,mx+mbc
	 xcell = xlower + (i-0.5d0)*dx
	 if (xcell .lt. 0.d0) then
	     aux(i,1) = 2.0d0
	    else
	     aux(i,1) = 1.0d0
	    endif

c        # pass aux array to out1.f in order to plot velocities:
         umax(i) = aux(i,1)

  150    continue

       return
       end
