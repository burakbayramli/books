      program zhabilg
c...Solves the Riemann problem for the Euler equations using first-order
c...upwind methods based on ZHA-BILGEN flux vector splitting.

c...Small amount of constant-coefficient artificial viscosity added to
c...help stabilize solution on some problems.

c...Number of grid points.
      parameter(N = 50)

      real*8 gamma, pl, pr, ul, ur, p, a, u, rul, rur, delta_t, delta_x
      real*8 aa, bb, t, rhol, rhor, R, cu, cp, lambda, cfl, x
      real*8 u1(0:N+2), u2(0:N+2), u3(0:N+2), retl, retr
      real*8 f1(0:N+1), f2(0:N+1), f3(0:N+1), me(1:N+1), al, ar
      real*8 av1(0:N+2), av2(0:N+2), av3(0:N+2)

      parameter (gamma=1.4, R= 287.0, cflfac = 0.4)
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
      write(*,*) 'Final time requested =  ', t
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

      t = 0 
C...Main loop.
      do 100, j=1,itert

         t = t + delta_t

         cfl = 0.
C...Find conserative numerical fluxes.
         do 40, i=0,N+1
           call zb(u1(i),u2(i),u3(i),u1(i+1),u2(i+1),
     *                  u3(i+1),f1(i),f2(i),f3(i))
           av1(i)=0.
           av2(i)=0.
           av3(i)=0.
c          if(i.gt.0) av1(i) = .03*(u1(i+1)-2.*u1(i)+u1(i-1))
c          if(i.gt.0) av2(i) = .03*(u2(i+1)-2.*u2(i)+u2(i-1))
c          if(i.gt.0) av3(i) = .03*(u3(i+1)-2.*u3(i)+u3(i-1))
 40      continue

C...Update conserved variables.
         do 50, i=1,N+1
           if(j.gt.2) then
           u1(i) = u1(i) - lambda*(f1(i)-f1(i-1))
           u2(i) = u2(i) - lambda*(f2(i)-f2(i-1))
           u3(i) = u3(i) - lambda*(f3(i)-f3(i-1))
           else
           u1(i) = u1(i) - lambda*(f1(i)-f1(i-1)) + av1(i)
           u2(i) = u2(i) - lambda*(f2(i)-f2(i-1)) + av2(i)
           u3(i) = u3(i) - lambda*(f3(i)-f3(i-1)) + av3(i)
           endif
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
      write(*,*) 'Final time = ', t 
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

      subroutine zb(rl,rul,retl,rr,rur,retr,f1,f2,f3)

c...Zha-Bilgen flux vector splitting.

      real*8 gamma, rl, rul, retl, rr, rur, retr, f1, f2, f3
      real*8 rhol, rhor, ul, ur, pl, pr, hl, hr, al, ar, Ml, Mr
      real*8 pp, pm, pap, pam

      parameter(gamma=1.4)

c...Convert conservative variables to primitive variables.
      rhol = rl
      rhor = rr
      ul = rul/rhol
      ur = rur/rhor
      pl = (gamma-1.)*(retl - .5*rul*rul/rhol)
      pr = (gamma-1.)*(retr - .5*rur*rur/rhor)
      hl = (retl+pl)/rhol
      hr = (retr+pr)/rhor
      al = sqrt(gamma*pl/rhol)
      ar = sqrt(gamma*pr/rhor)
      Ml = ul/al
      Mr = ur/ar

c...Compute positive splitting of p.
      if(Ml.le.-1.) then
        pp = 0.
        pap = 0.
      elseif(Ml.lt.1.) then
        pp = .5*(1.+Ml)*pl
        pap = .5*(ul+al)*pl
      else
        pp = pl
        pap = pl*ul
      endif

c...Compute negative splitting of M and p.
      if(Mr.le.-1.) then
        pm = pr
        pam = pr*ur
      elseif(Mr.lt.1.) then
        pm = .5*(1.-Mr)*pr
        pam =.5*(ur-ar)*pr
      else
        pm = 0.
        pam = 0.
      endif

c...Compute conserative numerical fluxes.

c     f1 = .5*(rul+rur)-.5*rhor*abs(ur) +.5*rhol*abs(ul)
c     f2 = .5*(rul*ul+rur*ur+pl+pr)-.5*rur*abs(ur)+.5*rul*abs(ul)
c    *     -.5*pr*Mr +.5*pl*Ml
c     f3 = .5*(rhor*hr*ur+rhol*hl*ul) -.5*retr*abs(ur) 
c    *     +.5*retl*abs(ul)-.5*pr*ar + .5*pl*al
      f1 = max(0.,ul)*rhol + min(0.,ur)*rhor
      f2 = max(0.,ul)*rul  + min(0.,ur)*rur + pp + pm
      f3 = max(0.,ul)*retl + min(0.,ur)*retr + pap + pam

      return
      end
