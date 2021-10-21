c     ============================================
      subroutine b4step1(maxmx,mbc,mx,meqn,q,
     &		  xlower,dx,t,dt,maux,aux)
c     ============================================
c
c     # called from claw1 before each call to step1.
c     # use to set time-dependent aux arrays or perform other tasks
c     # which must be done every time step.
c
c
c     
      implicit double precision (a-h,o-z)
      dimension q(1-mbc:maxmx+mbc, meqn)
      dimension aux(1-mbc:maxmx+mbc, *)
      common /comlxf/ alxf

c     # coefficient needed in rp1lxf for Lax-Friedrichs:
      alxf = dx / dt
c
      return
      end
