
c
c
c     =================================================================
      subroutine bc1(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux,t,dt,mthbc)
c     =================================================================
c
c     # Standard boundary condition choices for claw2
c
c     # Modified for layered medium:
c     #   For small t oscillating solid wall BCs are used to generate pulse
c     #   For t>50, switch to periodic boundary conditions.  The code
c     #     can then be run to much larger t to observe how the pulse
c     #     behaves as it loops around.
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
      common /comwall/ pi,t1,a1,tw1,t2,a2,tw2

c
c     # switch to periodic boundary conditions once pulse is generated:
      if (t .gt. 50.d0 .and. t .lt. 51.d0) then
         mthbc(1) = 2
         mthbc(2) = 2
	 do m=1,meqn
	    do ibc=1,mbc
	       aux(1-ibc,m) = aux(mx+1-ibc,m)
	       enddo
	    enddo
	 endif
     
c
c-------------------------------------------------------
c     # left boundary:
c-------------------------------------------------------
      go to (100,110,120,130) mthbc(1)+1
c
  100 continue
c     # user-specified boundary conditions 
c     # oscillating wall 
      do 105 m=1,meqn
         do 105 ibc=1,mbc
               q(1-ibc,m) = q(ibc,m)
  105       continue
c     # wall velocity:
      vwall = a1*g0((t-t1)/tw1)
c     # adjust the normal velocity:
      do 106 ibc=1,mbc
            q(1-ibc,2) = 2.0d0*vwall - q(ibc,2)
  106    continue
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
c     # user-specified boundary conditions 
c     # oscillating wall 
      do 205 m=1,meqn
         do 205 ibc=1,mbc
	       q(mx+ibc,m) = q(mx+1-ibc,m)
  205       continue
c     # wall velocity:
      vwall = a2*g0((t-t2)/tw2)
c     # adjust the normal velocity:
      do 206 ibc=1,mbc
         q(mx+ibc,2) = 2.0d0*vwall - q(mx+1-ibc,2)
  206    continue
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


c     ===============================
      double precision function g0(t)
c     ===============================

      implicit double precision (a-h,o-z)
      common /comwall/ pi,t1,a1,t2,a2,tw1,tw2

      if (dabs(t) .lt. 1.d0)  then
	  g0 = 1.d0 + dcos(pi*t)
	else
	  g0 = 0.d0
	endif

      return
      end

