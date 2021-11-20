      program godunov
c...Solves the Riemann problem for the Euler equations using Godunov's 
c...first-order upwind method

c...Number of grid points.
      parameter(N = 50)

      real*8 gamma, pl, pr, ul, ur, p, a, u, rul, rur, delta_t, delta_x
      real*8 aa, bb, t, rhol, rhor, R, cu, cp, lambda, cfl, x
      real*8 u1(0:N+2), u2(0:N+2), u3(0:N+2), retl, retr
      real*8 f1(0:N+1), f2(0:N+1), f3(0:N+1), me(1:N+1), al, ar

      parameter (gamma=1.4,cflfac = 0.4)
      parameter (R = 287.0, cu = R/(gamma-1), cp = gamma*R/(gamma-1.))

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
     *                  u3(i+1),f1(i),f2(i),f3(i),i,j)
           if(f2(i).lt.0.) then
             write(*,*) 'WARNING: Negative momentum flux'
             write(*,*) '# time steps = ', j
             write(*,*) 'Grid point = ', i
             write(*,*) 'x = ', aa + (bb-aa)*real(i-1)/real(N)
             write(*,*) 'Momentum flux = ', f2(i)
             stop
         endif
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
 50   continue
      if(cfl.gt.1.)  then
         write(*,*) 'WARNING: CFL condition violated'
         write(*,*) '# time steps = ', j
         write(*,*) 'CFL number = ', cfl
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

      subroutine riemann(r4,ru4,ret4,r1,ru1,ret1,f1,f2,f3,ng,nt)

      real*8 gamma, p1, p2, p3, p4, u1, u2, u3, u4, f1, f2, f3
      real*8 a1, a2, a3, a4, p, x, y, fg, ru1, ru4, ret1, ret4
      real*8 du, fx, fy, u, a, rho1, rho2, rho3, rho4
      real*8 tol, g1, g2, g3, t1, t2, t3, R, cu, cp, r1, r4
      real*8 s1, s2, s3, s4, rho, z, fz
      logical revflag, converge

      parameter(gamma=1.4,g1=.5*(gamma-1.)/gamma,g2=.5*(gamma+1.)/gamma)
      parameter(g3=(gamma+1.)/(gamma-1.))
      parameter(tol=1.E-10)
      parameter (R = 287.0, cu = R/(gamma-1.), cp = gamma*R/(gamma-1.))

      fg(x) = (x-1.) / sqrt(g2*(x-1.)+1.)

c...Convert conservative variables to primitive variables.
      rho1 = r1
      rho4 = r4
      u1 = ru1/rho1
      u4 = ru4/rho4
      p1 = (gamma-1.)*(ret1 - .5*ru1*ru1/rho1)
      p4 = (gamma-1.)*(ret4 - .5*ru4*ru4/rho4)

c...If necessary, switch states 1 and 4 so that high pressure
c...is on left and low pressure is on right
      revflag = .false.
      if(p4.lt.p1) then
        t1 = p1
        t2 = u1
        t3 = rho1
        p1 = p4
        u1 = -u4
        rho1 = rho4
        p4 = t1
        u4 = -t2
        rho4 = t3
        revflag = .true.
      endif

      a1 = sqrt(gamma*p1/rho1)
      a4 = sqrt(gamma*p4/rho4)
      p = (p4/p1)**g1
      du = u4 - u1

c...Apply the secant method
c...Initial guesses.
      x = .05*p4/p1
      y = .5*p4/p1
      fx = p - (x**g1) / (1.+g1*(gamma*du-a1*fg(x))/a4 )
      fy = p - (y**g1) / (1.+g1*(gamma*du-a1*fg(y))/a4 )
      converge = .false.

      do 10, i = 1, 20

        z = y - fy*(y-x) / (fy-fx)
        fz = p - (z**g1) / (1.+g1*(gamma*du-a1*fg(z))/a4 )
 
        if(abs(fz).lt.tol.and.abs(z-y).lt.tol) then 
          converge = .true.
          goto 20
        endif
        
        x = y
        fx = fy
        y = z
        fy = fz

 10   continue
      
 20   if(.NOT.converge) then
        write (*, *) 'root solver failed to converge'
        write (*, *) '# time steps = ', nt
        write (*, *) 'grid point = ', ng
        write (*, *) '# iterations taken by root solver = ', i
        write (*, *) 'Left and right states as follows (rho,u,p):'
        write (*, *) r4, ru4, ret4
        write (*, *) r1, ru1, ret1
        write (*, *) 'Current results of root solver as follows:'
        write (*, *) x, fx
        write (*, *) y, fy
        stop
      endif

      x = z

c...Compute shock
      p2 = p1*x
      u2 = u1 + a1*fg(x)/gamma
c     u2 = u4 + 2.*a4*(1.-(x**g1)/p)/(gamma-1.)
      a2 = a1*sqrt(x*(g3+x)/(1.+g3*x) )
      rho2 = gamma*p2/(a2*a2)
      s1 = u1+a1*sqrt( g2*( x-1.) +1. )
c     s1 = (rho1*u1 - rho2*u2)/(rho1-rho2)

c...Compute contact
      p3 = p2
      u3 = u2
      a3 = a4+.5*(gamma-1.)*(u4-u3)
      s2 = u2
      rho3 = gamma*p3/(a3*a3)

c...Compute expansion
      s3 = u3-a3
      s4 = u4-a4

c...Compute fluxes.
      if(revflag) then
        if(s4.gt.0.) then
          f1 = -rho4*u4
          f2 = rho4*u4*u4 + p4
          f3 = -.5*rho4*u4*u4*u4 - rho4*a4*a4*u4/(gamma-1.)
        elseif(s3.gt.0.) then
          u = (-(gamma-1.)*u4+2.*a4)/(gamma+1.)
          a = u 
          p = p4*(a/a4)**(2.*gamma/(gamma-1.))
          if(a.lt.0..or.p.lt.0.) then
            write(*,*) 'Negative a or p'
            stop
          endif
          rho = gamma*p/(a*a)
          f1 = -rho*u
          f2 = rho*u*u + p 
          f3 = -.5*rho*u*u*u - rho*a*a*u/(gamma-1.)
        elseif(s2.gt.0.) then
          f1 = -rho3*u3
          f2 = rho3*u3*u3 + p3
          f3 =  -.5*rho3*u3*u3*u3 - rho3*a3*a3*u3/(gamma-1.)
        elseif(s1.gt.0.) then
          f1 = -rho2*u2
          f2 = rho2*u2*u2 + p2
          f3 = -.5*rho2*u2*u2*u2 - rho2*a2*a2*u2/(gamma-1.)
        else
          f1 = -rho1*u1
          f2 = rho1*u1*u1 + p1
          f3 = -.5*rho1*u1*u1*u1 - rho1*a1*a1*u1/(gamma-1.)
        endif
      else
        if(s4.gt.0.) then
          f1 = rho4*u4
          f2 = rho4*u4*u4 + p4
          f3 = .5*rho4*u4*u4*u4 + rho4*a4*a4*u4/(gamma-1.)
        elseif(s3.gt.0.) then
          u = ((gamma-1.)*u4+2.*a4)/(gamma+1.)
          a = u 
          p = p4*(a/a4)**(2.*gamma/(gamma-1.))
          if(a.lt.0..or.p.lt.0.) then
            write(*,*) 'Negative a or p'
            stop
          endif
          rho = gamma*p/(a*a)
          f1 = rho*u
          f2 = rho*u*u + p 
          f3 = .5*rho*u*u*u + rho*a*a*u/(gamma-1.)
        elseif(s2.gt.0.) then
          f1 = rho3*u3
          f2 = rho3*u3*u3 + p3
          f3 =  .5*rho3*u3*u3*u3 + rho3*a3*a3*u3/(gamma-1.)
        elseif(s1.gt.0.) then
          f1 = rho2*u2
          f2 = rho2*u2*u2 + p2
          f3 = .5*rho2*u2*u2*u2 + rho2*a2*a2*u2/(gamma-1.)
        else
          f1 = rho1*u1
          f2 = rho1*u1*u1 + p1
          f3 = .5*rho1*u1*u1*u1 + rho1*a1*a1*u1/(gamma-1.)
        endif
      endif

      return
      end
