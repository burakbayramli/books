
c
c
c     =====================================================
      subroutine bc1(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux,t,dt,mthbc)
c     =====================================================
c
c     # Standard boundary condition choices for claw2
c
c     # At each boundary  k = 1 (left),  2 (right):
c     #   mthbc(k) =  0  for user-supplied BC's (must be inserted!)
c     #            =  1  for zero-order extrapolation
c     #            =  2  for periodic boundary coniditions
c     #            =  3  for solid walls, assuming this can be implemented
c     #                  by reflecting the data about the boundary and then
c     #                  negating the 2'nd component of q.
c     ------------------------------------------------
c
c     # Extend the data from the computational region
c     #      i = 1, 2, ..., mx2
c     # to the virtual cells outside the region, with
c     #      i = 1-ibc  and   i = mx+ibc   for ibc=1,...,mbc
c
      implicit double precision (a-h,o-z)
      dimension q(1-mbc:maxmx+mbc, meqn)
      dimension aux(1-mbc:maxmx+mbc, *)

      dimension mthbc(2)
      common /comrp/ u

c
c
c-------------------------------------------------------
c     # left boundary:
c-------------------------------------------------------
      go to (100,110,120,130) mthbc(1)+1
c
  100 continue

c     # user-specified boundary conditions go here in place of error output
c     # for inflow,  q(xlower,t) = g(t)

      t0 = t + dx/(2.d0*u)
      q(0,1) = g(t0)
      tm1 = t + 3.d0*dx/(2.d0*u)
      q(-1,1) = g(tm1)
      go to 199
c
  110 continue
c     # zero-order extrapolation:
      do 115 m=1,meqn
         do 115 ibc=1,mbc
               q(1-ibc,m) = q(1,m)
  115       continue
      go to 199

  120 continue
c     # periodic:  
      do 125 m=1,meqn
         do 125 ibc=1,mbc
               q(1-ibc,m) = q(mx+1-ibc,m)
  125       continue
      go to 199

  130 continue
c     # solid wall (assumes 2'nd component is velocity or momentum in x):
      do 135 m=1,meqn
         do 135 ibc=1,mbc
               q(1-ibc,m) = q(ibc,m)
  135       continue
c     # negate the normal velocity:
      do 136 ibc=1,mbc
            q(1-ibc,2) = -q(ibc,2)
  136    continue
      go to 199

  199 continue

c
c-------------------------------------------------------
c     # right boundary:
c-------------------------------------------------------
      go to (200,210,220,230) mthbc(2)+1
c
  200 continue
c     # user-specified boundary conditions go here in place of error output
      write(6,*) '*** ERROR *** mthbc(2)=0 and no BCs specified in bc1'
      stop
      go to 299

  210 continue
c     # zero-order extrapolation:
      do 215 m=1,meqn
         do 215 ibc=1,mbc
               q(mx+ibc,m) = q(mx,m)
  215       continue
      go to 299

  220 continue
c     # periodic:  
      do 225 m=1,meqn
         do 225 ibc=1,mbc
               q(mx+ibc,m) = q(ibc,m)
  225       continue
      go to 299

  230 continue
c     # solid wall (assumes 2'nd component is velocity or momentum in x):
      do 235 m=1,meqn
         do 235 ibc=1,mbc
               q(mx+ibc,m) = q(mx+1-ibc,m)
  235       continue
      do 236 ibc=1,mbc
            q(mx+ibc,2) = -q(mx+1-ibc,2)
  236    continue
      go to 299

  299 continue
c
      return
      end
