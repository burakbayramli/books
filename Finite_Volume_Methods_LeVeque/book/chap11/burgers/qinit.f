c
c
c =========================================================
       subroutine qinit(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux)
c =========================================================
c
c     # Set initial conditions for q.
c
      implicit double precision (a-h,o-z)
      dimension q(1-mbc:maxmx+mbc, meqn)
      dimension aux(1-mbc:maxmx+mbc, *)
c
c
      pi = 4.d0*datan(1.0d0)
      do 150 i=1,mx
         xcell = xlower + (i-0.5d0)*dx
         q(i,1) = 0.d0
         if (xcell .gt. -pi .and. xcell .lt. pi) then
           q(i,1) = 2.0d0*dsin(3.d0*xcell) + (dcos(2.d0*xcell)+0.2d0)
           q(i,1) = q(i,1) * (dcos(xcell)+1.d0)
           endif
  150    continue
c
      return
      end
