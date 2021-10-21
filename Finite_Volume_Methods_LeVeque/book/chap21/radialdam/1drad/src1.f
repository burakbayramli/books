

c
c
c =========================================================
      subroutine src1(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux,t,dt)
c =========================================================
      implicit real*8(a-h,o-z)
      dimension q(1-mbc:maxmx+mbc, meqn)
c
      common /comrp/ grav
      common /comsrc/ ndim
c
c     # source terms for radial symmetry in shallow water equations
c
c     # ndim should be set in setprob.f
c     # ndim = 2  for cylindrical symmetry
c     # ndim = 3  for spherical symmetry
c
c     # 2-stage Runge-Kutta method
c
      do 10 i=1,mx+mbc
	 xcell = xlower + (i-0.5d0)*dx
	 qstar1 = q(i,1) - 0.5d0*dt*(ndim-1)/xcell * q(i,2)
 	 qstar2 = q(i,2) - 0.5d0*dt*(ndim-1)/xcell * q(i,2)**2 / q(i,1)
c
	 q(i,1) = q(i,1) - dt*(ndim-1)/xcell * qstar2
 	 q(i,2) = q(i,2) - dt*(ndim-1)/xcell * qstar2**2 / qstar1
   10    continue
c
      return
      end
