c     ============================================
      subroutine b4step2(maxmx,maxmy,mbc,mx,my,meqn,q,
     &            xlower,ylower,dx,dy,t,dt,maux,aux)
c     ============================================
c
c     # called from claw2 before each call to step2.
c     # use to set time-dependent aux arrays or perform other tasks
c     # which must be done every time step.
c
c     
      implicit double precision (a-h,o-z)
      dimension q(1-mbc:maxmx+mbc,1-mbc:maxmy+mbc, meqn)
      dimension aux(1-mbc:maxmx+mbc,1-mbc:maxmy+mbc, 7)
c
c
c     # integrate velocities to get displacements:

      do i=0,mx+1
         do j=0,my+1
            aux(i,j,6) = aux(i,j,6) + dt*q(i,j,4)
            aux(i,j,7) = aux(i,j,7) + dt*q(i,j,5)
            enddo
         enddo

      return
      end
