c
c
c     =====================================================
      subroutine rpt2(ixy,maxm,meqn,mwaves,mbc,mx,
     &                  ql,qr,aux1,aux2,aux3,
     &                  imp,asdq,bmasdq,bpasdq)
c     =====================================================
      implicit double precision (a-h,o-z)
c
c     # Riemann solver in the transverse direction for the acoustics equations.
c
c     # Split asdq into down-going flux bmasdq and up-going flux bpasdq.
c
      dimension     ql(1-mbc:maxm+mbc, meqn)
      dimension     qr(1-mbc:maxm+mbc, meqn)
      dimension   asdq(1-mbc:maxm+mbc, meqn)
      dimension bmasdq(1-mbc:maxm+mbc, meqn)
      dimension bpasdq(1-mbc:maxm+mbc, meqn)
c
c     # density, bulk modulus, and sound speed, and impedence of medium:
c     # (should be set in setprob.f)
      common /cparam/ rho,bulk,cc,zz

c
      if (ixy.eq.1) then
          mu = 2
          mv = 3
        else
          mu = 3
          mv = 2
        endif
c
      do 20 i = 2-mbc, mx+mbc
         a1 = (-asdq(i,1) + zz*asdq(i,mv)) / (2.d0*zz)
         a2 = (asdq(i,1) + zz*asdq(i,mv)) / (2.d0*zz)
c
c        # The down-going flux difference bmasdq is the product  -c * wave
c
         bmasdq(i,1) = cc * a1*zz
         bmasdq(i,mu) = 0.d0
         bmasdq(i,mv) = -cc * a1
c
c        # The up-going flux difference bpasdq is the product  c * wave
c
         bpasdq(i,1) = cc * a2*zz
         bpasdq(i,mu) = 0.d0
         bpasdq(i,mv) = cc * a2
c
   20    continue
c
      return
      end
