      program lf
c...Solves the Riemann problem for the Euler equations using
c...the Lax-Friedrichs method.

c...TSTEP = 1      Fixed time step 
c...TSTEP = 2      Fixed CFL number 

c...Number of grid points.
      parameter(N = 50)

      real*8 gamma, pl, pr, ul, ur, p, a, u, rul, rur, delta_t, delta_x
      real*8 aa, bb, t, rhol, rhor, R, cu, cp, lambda, rhomax, x, al, ar
      real*8 u1(-2:N+4), u2(-2:N+4), u3(-2:N+4), retl, retr, rho(1:N+1)
      real*8 ub1(-2:N+4), ub2(-2:N+4), ub3(-2:N+4)
      real*8 f1(-2:N+3), f2(-2:N+3), f3(-2:N+3)
      integer tstep

      parameter (gamma=1.4, R= 287.0, cfl = 0.4, tstep=1)
      parameter (cu = R/(gamma-1), cp = gamma*R/(gamma-1.))

      open (unit=11,file='input.dat')
      read(11,*) aa, bb, t
      read(11,*) pl, rhol, ul
      read(11,*) pr, rhor, ur
      close(unit=11)

      al = sqrt(gamma*pl/rhol)
      ar = sqrt(gamma*pr/rhor)
      delta_x = (bb-aa)/real(N)
      rhomax = max(abs(ul+al),abs(ur+ar),abs(ul-al),abs(ur-ar))
      delta_t = cfl*delta_x/rhomax
      write(*,*) delta_t
      if(tstep.eq.1) then
        itert = nint(t/delta_t)
      elseif(tstep.eq.2) then
        itert = 10000
      endif
      lambda = delta_t/delta_x
      write(*,*) 'Final time requested =  ', t
      write(*,*) 'Delta_t =  ', delta_t
      write(*,*) 'Delta_x =  ', delta_x
      write(*,*) 'Lambda =  ', lambda
      write(*,*) 'Initial CFL number =  ', lambda*rhomax
      if(tstep.eq.1) write(*,*) '# iterations =  ', itert

c...Convert primitive variables to conservative variables.
      rul = rhol*ul
      rur = rhor*ur
      retl = .5*rul*ul + pl/(gamma-1.)
      retr = .5*rur*ur + pr/(gamma-1.)

c...Construct the initial conditions for the Riemann problem.
      do 30, i=-2,N+4
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

      tt = 0.
C...Main loop.
      do 100, j=1,itert

         rhomax = 0.

         do 40, i=-2,N+3
           u = u2(i)/u1(i)
           p = (gamma-1.)*(u3(i) - .5*u2(i)*u2(i)/u1(i))
           f1(i) = u2(i)        
           f2(i) = u2(i)*u + p 
           f3(i) = (u3(i)+p)*u 
 40      continue

         do 50, i=1,N+1
           ub1(i) = .5*(u1(i+1)+u1(i-1)-lambda*(f1(i+1)-f1(i-1)))
           ub2(i) = .5*(u2(i+1)+u2(i-1)-lambda*(f2(i+1)-f2(i-1)))
           ub3(i) = .5*(u3(i+1)+u3(i-1)-lambda*(f3(i+1)-f3(i-1)))
           if(ub1(i).lt.0..or.ub3(i).lt.0.) then
             write(*,*) 'WARNING: Negative density/energy'
             write(*,*) '# time steps = ', j
             write(*,*) 'Grid point = ', i
             write(*,*) 'x = ', aa + (bb-aa)*real(i-1)/real(N)
             write(*,*) 'Density = ', u1(i)
             write(*,*) 'Total energy per unit volume = ', u3(i)
             stop
           endif
           u = ub2(i)/ub1(i)
           p = (gamma-1.)*(ub3(i) - .5*ub2(i)*ub2(i)/ub1(i))
           if(p.lt.0.) then
             write(*,*) 'WARNING: Negative pressure'
             write(*,*) '# time steps = ', j
             write(*,*) 'Grid point = ', i
             write(*,*) 'x = ', aa + (bb-aa)*real(i-1)/real(N)
             write(*,*) 'Pressure = ', p
             stop
           endif
           a = sqrt(gamma*p/ub1(i))
           rho(i) = max(abs(u+a),abs(u-a))
           rhomax = max(rhomax,rho(i))
 50      continue

         do 60, i=1,N+1
           u1(i) = ub1(i)
           u2(i) = ub2(i)
           u3(i) = ub3(i)
 60     continue

        tt = tt + delta_t
        if(tstep.eq.1.and.lambda*rhomax.gt.1.) then
          write(*,*) 'WARNING: CFL condition violated'
          write(*,*) '# time steps = ', j
          write(*,*) 'Maximum CFL number = ', lambda*rhomax
        elseif(tstep.eq.2) then
          if(abs(tt-t).gt..00000001) then
            delta_t = cfl*delta_x/rhomax
            if(tt+delta_t.ge.t) then
             delta_t = t - tt
            endif
            lambda = delta_t/delta_x
          else
            goto 200
          endif
        endif

 100  continue
      
 200  write(*,*) 'Calculation complete.'
      write(*,*) 'Final time = ', t 
      if(tstep.eq.2) write(*,*) '# iterations =  ', j
      if(tstep.eq.1) write(*,*) 'Final CFL number = ' , lambda*rhomax

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
        write(20,*) x, lambda*rho(i)
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
