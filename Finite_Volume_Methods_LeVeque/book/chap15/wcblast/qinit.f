c
c
c =========================================================
       subroutine qinit(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux)
c =========================================================
c
c     # Set initial conditions for q.
c     # Smooth entropy wave hitting a shock
c
c
      implicit double precision (a-h,o-z)
      dimension q(1-mbc:maxmx+mbc, meqn)
      dimension aux(1-mbc:maxmx+mbc, *)
      common /param/ gamma, gamma1
c
c
c
      do 150 i=1,mx
             xcell = xlower + (i-0.5d0)*dx
             q(i,1) = 1.d0
             q(i,2) = 0.d0
             if (xcell .lt. 0.1d0) then
                 q(i,3) = 1.d3/gamma1
                else if (xcell .lt. 0.9d0) then
                 q(i,3) = 1.d-2/gamma1
                else
                 q(i,3) = 1.d2/gamma1
                endif

  150    continue
c
      return
      end
