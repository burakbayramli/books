      program lw
c...Solves the Riemann problem for the Euler equations using
c...the MacCormack or Richtmyer methods.

c...METHOD = 1     MacCormack
c...METHOD = 2     Richtmyer

c...TSTEP = 1      Fixed time step 
c...TSTEP = 2      Fixed CFL number 

c...A very small amount of fixed-coefficient artificial visocisty
c...is added to stabilize the calculation, necessary except for
c...large CFL numbers (approximately between 0.8 and 1.0).

c...Number of grid points.
      parameter(N = 50)

      real*8 gamma, pl, pr, ul, ur, p, a, u, rul, rur, delta_t, delta_x
      real*8 aa, bb, t, rhol, rhor, R, cu, cp, lambda, rhomax, x, al, ar
      real*8 u1(-2:N+4), u2(-2:N+4), u3(-2:N+4), retl, retr, rho(1:N+1)
      real*8 ub1(-2:N+4), ub2(-2:N+4), ub3(-2:N+4), tt
      real*8 f1(-2:N+3), f2(-2:N+3), f3(-2:N+3)
      real*8 fb1(-1:N+2), fb2(-1:N+2), fb3(-1:N+2)
      real*8 av1(-2:N+3), av2(-2:N+3), av3(-2:N+3)
      integer method, tstep

      parameter (gamma=1.4, R= 287.0, cfl = 0.4, method=1, tstep=1)
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
          ub1(i) = rhol
          ub2(i) = rul
          ub3(i) = retl
        elseif(x.ge.0.) then
          u1(i) = rhor
          u2(i) = rur
          u3(i) = retr
          ub1(i) = rhor
          ub2(i) = rur
          ub3(i) = retr
        endif
 30   continue

      tt = 0.
C...Main loop.
      do 100, j=1,itert

         rhomax = 0.
C...Find PREDICTOR fluxes.
         if(method.eq.1) then
           do 40, i=-2,N+3
             call flux(u1(i),u2(i),u3(i),u1(i+1),u2(i+1),
     *       u3(i+1),f1(i),f2(i),f3(i),1)
 40        continue
         elseif(method.eq.2) then
           do 41, i=-2,N+3
             call flux(u1(i),u2(i),u3(i),u1(i+1),u2(i+1),
     *       u3(i+1),f1(i),f2(i),f3(i),1)
 41        continue
         endif

C...Find PREDICTED solution.
         if(method.eq.1) then
           do 50, i=-1,N+3
             ub1(i) = u1(i) - lambda*(f1(i)-f1(i-1))
             ub2(i) = u2(i) - lambda*(f2(i)-f2(i-1))
             ub3(i) = u3(i) - lambda*(f3(i)-f3(i-1))
             if(j.eq.1.or.j.eq.2) then
               av1(i) = .02*(u1(i+1)-2.*u1(i)+u1(i-1))
               av2(i) = .02*(u2(i+1)-2.*u2(i)+u2(i-1))
               av3(i) = .02*(u3(i+1)-2.*u3(i)+u3(i-1))
             else
               av1(i) = 0.
               av2(i) = 0.
               av3(i) = 0.
             endif
c             av1(i) = .02*(u1(i+1)-2.*u1(i)+u1(i-1))
c             av2(i) = .02*(u2(i+1)-2.*u2(i)+u2(i-1))
c             av3(i) = .02*(u3(i+1)-2.*u3(i)+u3(i-1))
 50        continue
         elseif(method.eq.2) then
           do 51, i=-1,N+3
             ub1(i) = .5*(u1(i)+u1(i+1)-lambda*(f1(i)-f1(i-1)))
             ub2(i) = .5*(u2(i)+u2(i+1)-lambda*(f2(i)-f2(i-1)))
             ub3(i) = .5*(u3(i)+u3(i+1)-lambda*(f3(i)-f3(i-1)))
             if(j.eq.1.or.j.eq.2) then
               av1(i) = .02*(u1(i+1)-2.*u1(i)+u1(i-1))
               av2(i) = .02*(u2(i+1)-2.*u2(i)+u2(i-1))
               av3(i) = .02*(u3(i+1)-2.*u3(i)+u3(i-1))
             else
               av1(i) = 0.
               av2(i) = 0.
               av3(i) = 0.
             endif
c             av1(i) = .02*(u1(i+1)-2.*u1(i)+u1(i-1))
c             av2(i) = .02*(u2(i+1)-2.*u2(i)+u2(i-1))
c             av3(i) = .02*(u3(i+1)-2.*u3(i)+u3(i-1))
 51        continue
         endif

C...Find CORRECTOR fluxes.
         if(method.eq.1) then
           do 60, i=-1,N+2
             call flux(ub1(i),ub2(i),ub3(i),ub1(i+1),ub2(i+1),ub3(i+1),
     *       fb1(i),fb2(i),fb3(i),0)
 60        continue
         elseif(method.eq.2) then
           do 61, i=-1,N+2
             call flux(ub1(i),ub2(i),ub3(i),ub1(i+1),ub2(i+1),ub3(i+1),
     *       fb1(i),fb2(i),fb3(i),0)
 61        continue
         endif

C...Find CORRECTED solution.
         if (method.eq.1) then
           do 70, i=1,N+1
             if(j.gt.itert) then
             u1(i) = .5*(u1(i)+ub1(i)-lambda*(fb1(i)-fb1(i-1)))
             u2(i) = .5*(u2(i)+ub2(i)-lambda*(fb2(i)-fb2(i-1)))
             u3(i) = .5*(u3(i)+ub3(i)-lambda*(fb3(i)-fb3(i-1)))
             else
             u1(i) = .5*(u1(i)+ub1(i)-lambda*(fb1(i)-fb1(i-1)))+av1(i)
             u2(i) = .5*(u2(i)+ub2(i)-lambda*(fb2(i)-fb2(i-1)))+av2(i)
             u3(i) = .5*(u3(i)+ub3(i)-lambda*(fb3(i)-fb3(i-1)))+av3(i)
             endif
 70        continue
         elseif (method.eq.2) then
           do 71, i=1,N+1
             if(j.gt.itert) then
             u1(i) = u1(i)-lambda*(fb1(i)-fb1(i-1))
             u2(i) = u2(i)-lambda*(fb2(i)-fb2(i-1))
             u3(i) = u3(i)-lambda*(fb3(i)-fb3(i-1))
             else
             u1(i) = u1(i)-lambda*(fb1(i)-fb1(i-1))+av1(i)
             u2(i) = u2(i)-lambda*(fb2(i)-fb2(i-1))+av2(i)
             u3(i) = u3(i)-lambda*(fb3(i)-fb3(i-1))+av3(i)
             endif
 71        continue
         endif
             
         do 80, i=1,N+1
           if(u1(i).lt.0..or.u3(i).lt.0.) then
             write(*,*) 'WARNING: Negative density/energy'
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
           rho(i) = max(abs(u+a),abs(u-a))
           rhomax = max(rhomax,rho(i))
 80     continue

        tt = tt + delta_t
        if(tstep.eq.1.and.lambda*rhomax.gt.1.) then
          write(*,*) 'WARNING: CFL condition violated'
          write(*,*) '# time steps = ', j
          write(*,*) 'Maximum CFL number = ', lambda*rhomax
        elseif(tstep.eq.2.and.tt.lt.t) then
          delta_t = cfl*delta_x/rhomax
          if(tt+delta_t.ge.t) then
            delta_t = t - tt
          endif
          lambda = delta_t/delta_x
        elseif(tstep.eq.2.and.tt.ge.t) then
          goto 200
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

      subroutine flux(rl,rul,retl,rr,rur,retr,f1,f2,f3,i)

c...Computes conservative numerical fluxes
c...i=EVEN  ---- FTBS
c...i=ODD   ---- FTFS

      real*8 gamma, rl, rul, retl, rr, rur, retr
      real*8 ul, ur, pl, pr, f1, f2, f3
      integer oddeven

      parameter(gamma=1.4)

      oddeven = i-2*(i/2)

      if(oddeven.eq.0) then
        ul = rul/rl
        pl = (gamma-1.)*(retl - .5*rul*rul/rl)
        f1 = rul
        f2 = rul*ul + pl
        f3 = (retl+pl)*ul
      elseif(oddeven.eq.1) then
        ur = rur/rr
        pr = (gamma-1.)*(retr - .5*rur*rur/rr)
        f1 = rur
        f2 = rur*ur + pr
        f3 = (retr+pr)*ur
      endif

      return
      end
