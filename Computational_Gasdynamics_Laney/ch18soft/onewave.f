      program onewave
c...Solves the Riemann problem for the Euler equations using a first-order
c...upwind 'reconstruction-evolution' method with an approximate
c...ONE WAVE Riemann solver.
c     NAVG = 1  (A(u)+A(v))/2
c     NAVG = 2  A((u+v)/2)
c     NAVG = 3  Roe average

c...Number of grid points.
      parameter(N = 50)

      real*8 gamma, pl, pr, ul, ur, p, a, u, rul, rur, delta_t, delta_x
      real*8 aa, bb, t, rhol, rhor, R, cu, cp, lambda, cfl, x
      real*8 u1(0:N+2), u2(0:N+2), u3(0:N+2), retl, retr
      real*8 f1(0:N+1), f2(0:N+1), f3(0:N+1), me(1:N+1), al, ar

      parameter (gamma=1.4, R= 287.0, cflfac = 0.4, navg = 2)
      parameter (cu = R/(gamma-1), cp = gamma*R/(gamma-1.))

      open (unit=11,file='input.dat')
      read(11,*) aa, bb, t
      read(11,*) pl, rhol, ul
      read(11,*) pr, rhor, ur
      close(unit=11)

      al = sqrt(gamma*pl/rhol)
      ar = sqrt(gamma*pr/rhor)
      delta_x = (bb-aa)/real(N)
      me(1) = max(abs(ul+al),abs(ur+ar),abs(ul-al),abs(ur-ar))
      delta_t = cflfac*delta_x/me(1)
      itert = nint(t/delta_t)
      lambda = delta_t/delta_x
      cfl = lambda*me(1)
      write(*,*) 'Final time =  ', t
      write(*,*) 'Delta_t =  ', delta_t
      write(*,*) 'Delta_x =  ', delta_x
      write(*,*) 'Lambda =  ', lambda
      write(*,*) 'Initial CFL number =  ', cfl
      write(*,*) '# iterations =  ', itert

c...Convert primitive variables to conservative variables.
      rul = rhol*ul
      rur = rhor*ur
      retl = .5*rul*ul + pl/(gamma-1.)
      retr = .5*rur*ur + pr/(gamma-1.)

c...Construct the initial conditions for the Riemann problem.
      do 30, i=0,N+2
        x = aa + (bb-aa)*real(i-1)/real(N)
        if(x.lt.0.) then
          u1(i) = rhol
          u2(i) = rul
          u3(i) = retl
        elseif(x.ge.0.) then
          u1(i) = rhor
          u2(i) = rur
          u3(i) = retr
        endif
 30   continue

C...Main loop.
      do 100, j=1,itert

         cfl = 0.
C...Find conserative numerical fluxes.
         do 40, i=0,N+1
           call riemann(u1(i),u2(i),u3(i),u1(i+1),u2(i+1),
     *                  u3(i+1),f1(i),f2(i),f3(i),navg)
 40      continue

C...Update conserved variables.
         do 50, i=1,N+1
           u1(i) = u1(i) - lambda*(f1(i)-f1(i-1))
           u2(i) = u2(i) - lambda*(f2(i)-f2(i-1))
           u3(i) = u3(i) - lambda*(f3(i)-f3(i-1))
           if(u1(i).lt.0..or.u3(i).lt.0.) then
             write(*,*) 'WARNING: Negative density or energy'
             write(*,*) '# time steps = ', j
             write(*,*) 'Grid point = ', i
             write(*,*) 'x = ', aa + (bb-aa)*real(i-1)/real(N)
             write(*,*) 'Density = ', u1(i)
             write(*,*) 'Total energy per unit volume = ', u3(i)
             stop
           endif
           u = u2(i)/u1(i)
           p = (gamma-1.)*(u3(i) - .5*u2(i)*u2(i)/u1(i))
           if(p.lt.0.) then
             write(*,*) 'WARNING: Negative pressure'
             write(*,*) '# time steps = ', j
             write(*,*) 'Grid point = ', i
             write(*,*) 'x = ', aa + (bb-aa)*real(i-1)/real(N)
             write(*,*) 'Pressure = ', p
             stop
           endif
           a = sqrt(gamma*p/u1(i))
           me(i) = max(abs(u+a),abs(u-a))
           cfl = max(cfl,lambda*me(i))
 50     continue

        if(cfl.gt.1.)  then
           write(*,*) 'WARNING: CFL condition violated'
           write(*,*) '# time steps = ', j
           write(*,*) 'Maximum CFL number = ', cfl
         endif

 100  continue
      
      write(*,*) 'Calculation complete.'
      write(*,*) 'Final CFL number = ' , cfl

      open (unit = 13, file = 'pressure.out')
      open (unit = 14, file = 'velocity.out')
      open (unit = 15, file = 'sound.out')
      open (unit = 16, file = 'density.out')
      open (unit = 17, file = 'entropy.out')
      open (unit = 18, file = 'mach.out')
      open (unit = 19, file = 'massflux.out')
      open (unit = 20, file = 'spectral.out')

c...Report results.
      do 110, i=1,N+1
        x = aa + (bb-aa)*real(i-1)/real(N)
        p = (gamma-1.)*(u3(i) - .5*u2(i)*u2(i)/u1(i))
        a = sqrt(gamma*p/u1(i))
        u = u2(i)/u1(i)
        write(13,*) x, p
        write(14,*) x, u
        write(15,*) x, a
        write(16,*) x, u1(i)
        write(17,*) x, cu*log(p)-cp*log(u1(i))
        write(18,*) x, u/a
        write(19,*) x, u2(i)
        write(20,*) x, lambda*me(i)
 110  continue
        
      close(unit=13)
      close(unit=14)
      close(unit=15)
      close(unit=16)
      close(unit=17)
      close(unit=18)
      close(unit=19)
      close(unit=20)
       
      stop
      end

      subroutine riemann(r4,ru4,ret4,r1,ru1,ret1,f1,f2,f3,navg)

c     NAVG = 1  (A(u)+A(v))/2
c     NAVG = 2  A((u+v)/2)
c     NAVG = 3  Roe average

      real*8 gamma, p1, p4, u1, u4, f1, f2, f3, ru1, ru4, ret1, ret4, rr
      real*8 r1, r4, rho1, rho4, h1, h4, ruavg, retavg, uavg, havg, aavg
      real*8 rhoavg

      parameter(gamma=1.4)

c...Convert conservative variables to primitive variables.
      rho1 = r1
      rho4 = r4
      u1 = ru1/rho1
      u4 = ru4/rho4
      p1 = (gamma-1.)*(ret1 - .5*ru1*ru1/rho1)
      p4 = (gamma-1.)*(ret4 - .5*ru4*ru4/rho4)
      h1 = (ret1+p1)/rho1
      h4 = (ret4+p4)/rho4

      if(navg.eq.1) then
        al = sqrt(gamma*p4/rho4)
        ar = sqrt(gamma*p1/rho1)
        specrad = max(abs(.5*(u4+al+u1+ar)),abs(.5*(u4-al+u1-ar)))
      elseif(navg.eq.2)  then
        rhoavg = .5*(rho4+rho1)
        ruavg = .5*(ru4+ru1)
        retavg = .5*(ret4+ret1)
        uavg = ruavg/rhoavg
        pavg = (gamma-1.)*(retavg - .5*ruavg*ruavg/rhoavg)
        aavg = sqrt(gamma*pavg/rhoavg)
        specrad = max(abs(uavg+aavg),abs(uavg-aavg))
      elseif(navg.eq.3)  then
        rr = sqrt(rho1/rho4)
        rhoavg = rr*rho4
        uavg = (rr*u1+u4)/(rr+1.)
        havg = (rr*h1+h4)/(rr+1.)
        aavg = sqrt((gamma-1.)*(havg-.5*uavg*uavg))
        specrad = max(abs(uavg+aavg),abs(uavg-aavg))
      endif

      f1 = .5*(ru4+ru1 - specrad*(rho1-rho4))
      f2 = .5*(ru4*u4+ru1*u1+p4+p1 - specrad*(ru1-ru4))
      f3 = .5*(rho4*h4*u4+rho1*h1*u1 - specrad*(ret1-ret4))

      return
      end

