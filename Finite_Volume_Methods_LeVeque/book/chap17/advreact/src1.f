
c
c
c =========================================================
      subroutine src1(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux,t,dt)
c =========================================================
      implicit double precision (a-h,o-z)
      dimension q(1-mbc:maxmx+mbc, meqn)
      common /comsrc/ rate
c
c     # solve q' = rate*q 
c     # use the exact solution operator
c
      sdt = dexp(-dt*rate)
      do i=1,mx
         q(i,1) = sdt * q(i,1)
	 enddo
c
      return
      end
