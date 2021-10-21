
c
c
c =========================================================
      subroutine src1(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux,t,dt)
c =========================================================
      implicit double precision (a-h,o-z)
      dimension q(1-mbc:maxmx+mbc, meqn)
c
c     # solve q' = -(1-x)*q 
c
      do i=1,mx
	 xcell = xlower + (i-0.5d0) * dx
	 rate = 1.d0 - xcell

c        # 2-stage Runge-Kutta method:
	 qstar = (1.d0 - 0.5d0*dt*rate) * q(i,1)
 	 q(i,1) = q(i,1) - dt*rate*qstar
c
c        # Note that one could instead use the exact solution operator
c        # for this source term:
c        q(i,1) = dexp(-dt*rate) * q(i,1)

	 enddo
c
      return
      end
