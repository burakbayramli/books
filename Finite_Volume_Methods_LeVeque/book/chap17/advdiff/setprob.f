      subroutine setprob
      implicit double precision (a-h,o-z)
      common /comrp/ u
      common /comsrc/ dcoef
c
c     # Set the velocity for scalar advection
c     # This value is passed to the Riemann solver rp1.f in a common block
c
c     # Set the diffusion coefficient for the source term
c     # dcoef is passed to src1.f in comsrc
c
      open(unit=7,file='setprob.data',status='old',form='formatted')

      read(7,*) u
      read(7,*) dcoef

      return
      end
