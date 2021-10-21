c
c
c     =====================================================
      subroutine rpt2(ixy,maxm,meqn,mwaves,mbc,mx,
     &                  ql,qr,aux1,aux2,aux3,
     &                  imp,asdq,bmasdq,bpasdq)
c     =====================================================
      implicit double precision (a-h,o-z)
c
c     # Riemann solver in the transverse direction for 2D Burgers' equation
c     #  u_t + cos(theta)*(0.5*u^2)_x + sin(theta)*(0.5*u^2)_y = 0
c
c     # Split asdq into eigenvectors of Roe matrix B.
c     # For the scalar equation, this simply amounts to computing the
c     # transverse wave speed from the opposite Riemann problem.
c
      dimension    ql(1-mbc:maxm+mbc, meqn)
      dimension    qr(1-mbc:maxm+mbc, meqn)
      dimension   asdq(1-mbc:maxm+mbc, meqn)
      dimension bmasdq(1-mbc:maxm+mbc, meqn)
      dimension bpasdq(1-mbc:maxm+mbc, meqn)
      common /comrp/ theta
c
      if (ixy .eq. 1) then
          b = 0.5d0*dsin(theta)
        else
          b = 0.5d0*dcos(theta)
        endif
c
          do 10 i = 2-mbc, mx+mbc
             sb = b*(qr(i-1,1) + ql(i,1))
             bmasdq(i,1) = dmin1(sb, 0.d0) * asdq(i,1)
             bpasdq(i,1) = dmax1(sb, 0.d0) * asdq(i,1)
   10        continue
c
      return
      end
