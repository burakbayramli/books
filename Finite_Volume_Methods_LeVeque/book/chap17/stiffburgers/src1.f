
c
c
c =========================================================
      subroutine src1(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux,t,dt)
c =========================================================
      implicit double precision (a-h,o-z)
      dimension q(1-mbc:maxmx+mbc, meqn)
c
c     # Burgers' equation with stiff source term
c     #       psi(u) = u(1-u)(u-beta) / tau
c

      external gtrap,gbdf2
      common /comsrc/  tau,beta,iode
      common /comode/ dtg,qn,psin,qstar
c
      parameter (msize = 5000)
      dimension c(msize),d(msize),e(msize),b(msize)


      go to (11,12,13) iode
c
   11 continue
c     # 2-stage explicit Runge-Kutta method for source terms
c
      do i=1-mbc,mx+mbc
 	 q1 = q(i,1)
         qstar = q1 + 0.5d0*dt*q1*(1.d0-q1)*(q1-beta) / tau
 	 q(i,1) = q1 + dt*qstar*(1.d0-qstar)*(qstar-beta)/tau
 	 enddo

      go to 19

   12 continue
c
c     # Trapezoidal for source terms:
c
      dtg = dt
      do i=1-mbc,mx+mbc
	 qn = q(i,1)
	 psin = psi(qn)
	 q(i,1) = zeroin(-1.d0, 2.d0, gtrap, 1d-8)
	 enddo
      go to 19

   13 continue
c
c     # TR-BDF2 for source terms
c     # half step with Trapezoidal followed by half step with BDF2
c
      dtg = 0.5d0 * dt
      do i=1-mbc,mx+mbc
 	 qn = q(i,1)
 	 psin = psi(qn)
 	 qstar = zeroin(-1.d0, 2.d0, gtrap, 1d-8)
 	 q(i,1) = zeroin(-1.d0, 2.d0, gbdf2, 1d-8)
 	 enddo

   19 continue
c 
      return 
      end

c    ==================================
      double precision function gtrap(q)
c    ==================================
      implicit double precision (a-h,o-z)
      external psi
      common /comode/ dtg,qn,psin,qstar

      gtrap = q - qn - 0.5d0*dtg*(psin + psi(q))
      return
      end


c    ==================================
      double precision function gbdf2(q)
c    ==================================
      implicit double precision (a-h,o-z)
      external psi
      common /comode/ dtg,qn,psin,qstar

      gbdf2 = 3.d0*q - 4.d0*qstar + qn - 2.d0*dtg*psi(q)
      return
      end


c    ==================================
      double precision function psi(q)
c    ==================================
      implicit double precision (a-h,o-z)
      common /comsrc/  tau,beta,iode
c
c     # source term:

      psi = q*(1.d0-q)*(q-beta) / tau
      return
      end
