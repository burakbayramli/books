

c
c
c =========================================================
      subroutine src1(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux,t,dt)
c =========================================================
      implicit real*8(a-h,o-z)
      dimension q(1-mbc:maxmx+mbc, meqn)
c
      common /cparam/ rho,bulk,cc,zz   
      common /comsrc/ ndim
c
c     # source terms for radial symmetry
c
c     # ndim should be set in setprob.f
c     # ndim = 2  for cylindrical symmetry
c     # ndim = 3  for spherical symmetry
c
      do 10 i=1,mx+mbc
	 xcell = xlower + (i-0.5d0)*dx
	 q(i,1) = q(i,1) - dt*(ndim-1)*bulk/xcell * q(i,2)
   10    continue
c
      return
      end
