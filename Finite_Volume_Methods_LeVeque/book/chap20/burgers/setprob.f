      subroutine setprob
      implicit double precision (a-h,o-z)
      common /comrp/ theta
c
c     # Burgers' equation at an angle theta to the grid,
c     #  u_t + cos(theta)*(0.5*u^2)_x + sin(theta)*(0.5*u^2)_y = 0
c
c     # Set theta for angle 
c     # Passed to the Riemann solver rp1.f in a common block
c
      open(unit=7,file='setprob.data',status='old',form='formatted')

      read(7,*) theta

      return
      end

