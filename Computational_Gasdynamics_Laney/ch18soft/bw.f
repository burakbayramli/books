       program beamwarm
c...Solves the Riemann problem for the Euler equations using
c...the Beam-Warming second-order upwind method based on any
c...of three possible flux vector splittings.

c...METHOD = 1     Steger-Warming
c...METHOD = 2     van Leer
c...METHOD = 3     Zha-Bilgen

c...TSTEP = 1      Fixed time step 
c...TSTEP = 2      Fixed CFL number

c...SMOOTH = .false.     Do not smooth initial conditions
c...SMOOTH = .true       Smooth initial conditions

c...ARTVIS = .false.     Do not use artificial viscosity
c...ARTVIS = .true.      Use artificial viscosity

c...Number of grid points.
      parameter(N = 50)

      integer method, tstep
	logical smooth, artvis
      real*8 gamma, pl, pr, ul, ur, p, a, u, rul, rur, delta_t, delta_x
      real*8 aa, bb, t, rhol, rhor, R, cu, cp, lambda, rhomax, x, al, ar
      real*8 u1(-2:N+4), u2(-2:N+4), u3(-2:N+4), retl, retr, rho(1:N+1)
      real*8 ub1(-2:N+4), ub2(-2:N+4), ub3(-2:N+4), tt
      real*8 fp1(-2:N+3), fp2(-2:N+3), fp3(-2:N+3)
      real*8 fm1(-2:N+3), fm2(-2:N+3), fm3(-2:N+3)
      real*8 fbp1(-1:N+2), fbp2(-1:N+2), fbp3(-1:N+2)
      real*8 fbm1(-1:N+2), fbm2(-1:N+2), fbm3(-1:N+2)
      real*8 av1(-2:N+4), av2(-2:N+4), av3(-2:N+4)

      parameter (gamma=1.4,R=287.0,cfl=0.4)
	parameter (method=3,tstep=1,smooth=.false.,artvis=.false.)
      parameter (cu = R/(gamma-1), cp = gamma*R/(gamma-1.))

      open (unit=11,file='input.dat')
      read(11,*) aa, bb, t
      read(11,*) pl, rhol, ul
      read(11,*) pr, rhor, ur
      close(unit=11)

      al = sqrt(gamma*pl/rhol)
      ar = sqrt(gamma*pr/rhor)
      rhomax = max(abs(ul+al),abs(ur+ar),abs(ul-al),abs(ur-ar))
      delta_x = (bb-aa)/real(N)
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
        if(.NOT.smooth) then
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
        elseif(smooth) then
c...Slightly smoothed initial data
          if(x.lt.-0.5001) then
            u1(i) = rhol
            u2(i) = rul
            u3(i) = retl
            ub1(i) = rhol
            ub2(i) = rul
            ub3(i) = retl
          elseif(x.le.-0.001) then
            u1(i) = .75*rhol+.25*rhor
            u2(i) = .75*rul+.25*rur
            u3(i) = .75*retl+.25*retr
            ub1(i) = .75*rhol+.25*rhor
            ub2(i) = .75*rul+.25*rur
            ub3(i) = .75*retl+.25*retr
          elseif(x.le.0.001) then
            u1(i) = .5*rhol+.5*rhor
            u2(i) = .5*rul+.5*rur
            u3(i) = .5*retl+.5*retr
            ub1(i) = .5*rhol+.5*rhor
            ub2(i) = .5*rul+.5*rur
            ub3(i) = .5*retl+.5*retr
          elseif(x.le.0.5001) then
            u1(i) = .25*rhol+.75*rhor
            u2(i) = .25*rul+.75*rur
            u3(i) = .25*retl+.75*retr
            ub1(i) = .25*rhol+.75*rhor
            ub2(i) = .25*rul+.75*rur
            ub3(i) = .25*retl+.75*retr
          else
            u1(i) = rhor
            u2(i) = rur
            u3(i) = retr
            ub1(i) = rhor
            ub2(i) = rur
            ub3(i) = retr
           endif
         endif
 30   continue

      tt = 0.
C...Main loop.
      do 100, j=1,itert

         rhomax = 0.
C...Find split PREDICTOR fluxes.
         if(method.eq.1) then
           do 40, i=-2,N+3
             call sw(u1(i),u2(i),u3(i),u1(i+1),u2(i+1),
     *       u3(i+1),fp1(i),fp2(i),fp3(i),fm1(i),fm2(i),fm3(i))
 40        continue
         elseif(method.eq.2) then
           do 41, i=-2,N+3
             call vl(u1(i),u2(i),u3(i),u1(i+1),u2(i+1),
     *       u3(i+1),fp1(i),fp2(i),fp3(i),fm1(i),fm2(i),fm3(i))
 41        continue
         elseif(method.eq.3) then
           do 42, i=-2,N+3
             call zb(u1(i),u2(i),u3(i),u1(i+1),u2(i+1),
     *       u3(i+1),fp1(i),fp2(i),fp3(i),fm1(i),fm2(i),fm3(i))
 42        continue
         endif

C...Find PREDICTED solution.
         do 50, i=-1,N+3
           ub1(i) = u1(i) - lambda*(fp1(i)+fm1(i)-fp1(i-1)-fm1(i-1))
           ub2(i) = u2(i) - lambda*(fp2(i)+fm2(i)-fp2(i-1)-fm2(i-1))
           ub3(i) = u3(i) - lambda*(fp3(i)+fm3(i)-fp3(i-1)-fm3(i-1))
           if(u1(i).lt.0..or.u3(i).lt.0.) then
             write(*,*) 'WARNING: Negative density/energy in predictor'
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
             write(*,*) 'WARNING: Negative pressure in predictor'
             write(*,*) '# time steps = ', j
             write(*,*) 'Grid point = ', i
             write(*,*) 'x = ', aa + (bb-aa)*real(i-1)/real(N)
             write(*,*) 'Pressure = ', p
             stop
           endif
 50      continue

C...Find split CORRECTOR fluxes.
         if(method.eq.1) then
           do 60, i=-1,N+2
             call sw(ub1(i),ub2(i),ub3(i),ub1(i+1),ub2(i+1),ub3(i+1),
     *       fbp1(i),fbp2(i),fbp3(i),fbm1(i),fbm2(i),fbm3(i))
 60        continue
         elseif(method.eq.2) then
           do 61, i=-1,N+2
             call vl(ub1(i),ub2(i),ub3(i),ub1(i+1),ub2(i+1),ub3(i+1),
     *       fbp1(i),fbp2(i),fbp3(i),fbm1(i),fbm2(i),fbm3(i))
 61        continue
         elseif(method.eq.3) then
           do 62, i=-1,N+2
             call zb(ub1(i),ub2(i),ub3(i),ub1(i+1),ub2(i+1),ub3(i+1),
     *       fbp1(i),fbp2(i),fbp3(i),fbm1(i),fbm2(i),fbm3(i))
 62        continue
         endif

C...Find CORRECTED solution.
         do 70, i=1,N+1
           if(artvis) then
             av1(i) = .05*(u1(i+1)-2.*u1(i)+u1(i-1))
             av2(i) = .05*(u2(i+1)-2.*u2(i)+u2(i-1))
             av3(i) = .05*(u3(i+1)-2.*u3(i)+u3(i-1))
           else
             av1(i) = 0.
             av2(i) = 0.
             av3(i) = 0.
           endif
           u1(i) = .5*(u1(i)+ub1(i))
     *             -.5*lambda*(fbp1(i)+fbm1(i)-fbp1(i-1)-fbm1(i-1))
     *             -.5*lambda*(fp1(i)-2.*fp1(i-1)+fp1(i-2))
     *             +.5*lambda*(fm1(i+1)-2.*fm1(i)+fm1(i-1))
     *             +av1(i)
           u2(i) = .5*(u2(i)+ub2(i))
     *             -.5*lambda*(fbp2(i)+fbm2(i)-fbp2(i-1)-fbm2(i-1))
     *             -.5*lambda*(fp2(i)-2.*fp2(i-1)+fp2(i-2))
     *             +.5*lambda*(fm2(i+1)-2.*fm2(i)+fm2(i-1))
     *             +av2(i)
           u3(i) = .5*(u3(i)+ub3(i))
     *             -.5*lambda*(fbp3(i)+fbm3(i)-fbp3(i-1)-fbm3(i-1))
     *             -.5*lambda*(fp3(i)-2.*fp3(i-1)+fp3(i-2))
     *             +.5*lambda*(fm3(i+1)-2.*fm3(i)+fm3(i-1))
     *             +av3(i)
           if(u1(i).lt.0..or.u3(i).lt.0.) then
             write(*,*) 'WARNING: Negative density/energy in corrector'
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
             write(*,*) 'WARNING: Negative pressure in corrector'
             write(*,*) '# time steps = ', j
             write(*,*) 'Grid point = ', i
             write(*,*) 'x = ', aa + (bb-aa)*real(i-1)/real(N)
             write(*,*) 'Pressure = ', p
             stop
           endif
           a = sqrt(gamma*p/u1(i))
           rho(i) = max(abs(u+a),abs(u-a))
           rhomax = max(rhomax,rho(i))
 70     continue

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

      subroutine sw(rl,rul,retl,rr,rur,retr,fp1,fp2,fp3,fm1,fm2,fm3)

c...Steger-Warming flux vector splitting.

      real*8 gamma, rl, rul, retl, rr, rur, retr
      real*8 rhol, rhor, ul, ur, pl, pr, hl, hr, al, ar
      real*8 lambda1p, lambda2p, lambda3p, lambda1m, lambda2m, lambda3m
      real*8 fp1, fp2, fp3, fm1, fm2, fm3

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

c...Compute wave speed splitting.

      lambda1p = max(0.,ul)
      lambda2p = max(0.,ul+al)
      lambda3p = max(0.,ul-al)

      lambda1m = min(0.,ur)
      lambda2m = min(0.,ur+ar)
      lambda3m = min(0.,ur-ar)

c...Compute flux splitting.
      fp1 = .5*rhol*(2.*(gamma-1.)*lambda1p+lambda2p+lambda3p)/gamma
      fp2 = .5*rhol*(2.*(gamma-1.)*ul*lambda1p+(ul+al)*lambda2p
     *                + (ul-al)*lambda3p)/gamma
      fp3 = .5*(gamma-1.)*rhol*lambda1p*ul*ul/gamma
     *       + .5*rhol*((hl+al*ul)*lambda2p+(hl-al*ul)*lambda3p)/gamma

      fm1 = .5*rhor*(2.*(gamma-1.)*lambda1m+lambda2m+lambda3m)/gamma
      fm2 = .5*rhor*(2.*(gamma-1.)*ur*lambda1m+(ur+ar)*lambda2m
     *                + (ur-ar)*lambda3m)/gamma
      fm3 = .5*(gamma-1.)*rhor*lambda1m*ur*ur/gamma
     *       + .5*rhor*((hr+ar*ur)*lambda2m+(hr-ar*ur)*lambda3m)/gamma

      return
      end

      subroutine vl(rl,rul,retl,rr,rur,retr,fp1,fp2,fp3,fm1,fm2,fm3)

c...Van Leer flux vector splitting.

      real*8 gamma, rl, rul, retl, rr, rur, retr
      real*8 rhol, rhor, ul, ur, pl, pr, hl, hr, al, ar, Ml, Mr
      real*8 Mp, Mm, tp, tm, fp1, fp2, fp3, fm1, fm2, fm3

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

c...Compute positive flux splitting.

      if(Ml.le.-1.) then
        fp1 = 0.
        fp2 = 0.
        fp3 = 0.
      elseif(Ml.lt.1.) then
        Mp = .25*(Ml+1.)*(Ml+1.)
        tp = (gamma-1.)*ul+2.*al
        fp1 = rhol*al*Mp
        fp2 = fp1*tp/gamma
        fp3 = .5*fp1*tp*tp/(gamma*gamma-1.)
      else
        fp1 = rul
        fp2 = rul*ul + pl
        fp3 = rhol*hl*ul
      endif

c...Compute negative flux splitting.

      if(Mr.le.-1.) then
        fm1 = rur
        fm2 = rur*ur + pr
        fm3 = rhor*hr*ur
      elseif(Mr.lt.1.) then
        Mm = -.25*(Mr-1.)*(Mr-1.)
        tm = (gamma-1.)*ur-2.*ar
        fm1 = rhor*ar*Mm
        fm2 = fm1*tm/gamma
        fm3 = .5*fm1*tm*tm/(gamma*gamma-1.)
      else
        fm1 = 0.
        fm2 = 0.
        fm3 = 0.
      endif

      return
      end

      subroutine zb(rl,rul,retl,rr,rur,retr,fp1,fp2,fp3,fm1,fm2,fm3)

c...Zha-Bilgen flux vector splitting.

      real*8 gamma, rl, rul, retl, rr, rur, retr, fp1, fp2, fp3
      real*8 rhol, rhor, ul, ur, pl, pr, hl, hr, al, ar, Ml, Mr
      real*8 pp, pm, pap, pam, fm1, fm2, fm3

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

      fp1 = max(0.,ul)*rhol
      fm1 = min(0.,ur)*rhor
      fp2 = max(0.,ul)*rul + pp
      fm2 = min(0.,ur)*rur + pm
      fp3 = max(0.,ul)*retl + pap
      fm3 = min(0.,ur)*retr + pam

      return
      end
