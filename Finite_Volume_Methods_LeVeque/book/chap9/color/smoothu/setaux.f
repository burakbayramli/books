c     ============================================
      subroutine setaux(maxmx,mbc,mx,xlower,dx,maux,aux)
c     ============================================
c
c     # set auxiliary arrays 
c     # aux(i,1) = velocity u_i in i'th cell for advection
c
c     # periodic smooth variation u = sin(2*pi*x), evaluated
c     # at cell centers
c
c     
      implicit double precision (a-h,o-z)
      dimension aux(1-mbc:maxmx+mbc, maux)

      pi2 = 8.d0*datan(1.d0)

      do i=1-mbc,mx+mbc
	 xi = xlower + (i-0.5d0)*dx
	 aux(i,1) = 1.d0 - 0.5d0 * dsin(pi2*xi)
	 enddo
c
       return
       end
