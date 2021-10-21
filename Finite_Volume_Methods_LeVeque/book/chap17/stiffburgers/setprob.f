c     ==================
      subroutine setprob
c     ==================
c
      implicit double precision (a-h,o-z)
c
c
      common /comsrc/  tau,beta,iode
      
      open(unit=7,file="setprob.data")


c     # ode method to use for source term:
c     #     iode = 1:   explicit 2-stage Runge-Kutta
c     #     iode = 2:   trapezoidal method
c     #     iode = 3:   TR-BDF2
      read(7,*) iode

c     # tau and beta for stiff source term:
      read(7,*) tau
      read(7,*) beta

      return
      end
