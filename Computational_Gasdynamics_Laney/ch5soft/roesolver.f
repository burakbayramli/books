      program roe
c...Find Roe's approximate solution to the Riemann problem for the
c...Euler equations.
      real*8 gamma, p1, p2, p3, p4, u1, u2, u3, u4
      real*8 a1, a2, a3, a4, aa, bb, t, x 
      real*8 rho1, rho2, rho3, rho4
      real*8 R, cu, cp, rr
      real*8 ent1, ent2, ent3, ent4
      real*8 mom1, mom2, mom3, mom4, re1, re2, re3, re4
      real*8 h1, h2, h3, h4, dvm, dvp, dv0
      real*8 lambda0, lambdam, lambdap

      parameter(gamma=1.4, R=287.0, N = 1000)
      parameter (cu = R/(gamma-1), cp = gamma*R/(gamma-1.))

      open ( unit=11, file='input.dat' )
      read( 11, *) aa, bb, t
      read( 11, *) p4, rho4, u4
      read( 11, *) p1, rho1, u1
      close(unit=11)

c... Compute state 1 (right state)
      mom1 = rho1*u1
      re1 = p1/(gamma-1.) + .5*mom1*u1
      h1 = (re1+p1)/rho1
      ent1 = cu*log(p1)-cp*log(rho1)
      a1 = sqrt(gamma*p1/rho1)

c... Compute state 4 (left state)
      mom4 = rho4*u4
      re4 = p4/(gamma-1) + .5*mom4*u4
      h4 = (re4+p4)/rho4
      ent4 = cu*log(p4)-cp*log(rho4)
      a4 = sqrt(gamma*p4/rho4)

c...Compute Roe averages
      rr = sqrt(rho1/rho4)
      rhoavg = rr*rho4
      uavg = (rr*u1+u4)/(rr+1.)
      havg = (rr*h1+h4)/(rr+1.)
      aavg = sqrt((gamma-1.)*(havg-.5*uavg*uavg))

c...Compute wavespeeds
      lambda0 = uavg
      lambdap = uavg + aavg
      lambdam = uavg - aavg

c...Compute delta_v
      dv0 = rho1 - rho4 - (p1 - p4)/(aavg*aavg)
      dvp = u1 - u4 + (p1 - p4 )/(rhoavg*aavg)
      dvm = u1 - u4 - (p1 - p4 )/(rhoavg*aavg)

c...Compute state 3
      rho3 = rho4 - .5*rhoavg*dvm/aavg
c     rho3 = rho1 - .5*rhoavg*dvp/aavg - dv0
      mom3 = mom4 - .5*rhoavg*lambdam*dvm/aavg
c     mom3 = mom1 - .5*rhoavg*lambdap*dvp/aavg - uavg*dv0
      re3  = re4 - .5*rhoavg*(havg-aavg*uavg)*dvm/aavg
c     re3 = re1 - .5*rhoavg*(havg+aavg*uavg)*dvp/aavg 
c     re3 = re3 - .5*uavg*uavg*dv0
      u3 = mom3/rho3
      p3 = (gamma-1.)*(re3-.5*mom3*u3)
      ent3 = cu*log(p3)-cp*log(rho3)
      a3 = sqrt(gamma*p3/rho3)
      h3 = (re3+p3)/rho3
      
c...Compute state2
      rho2 = rho1 - .5*rhoavg*dvp/aavg
c     rho2 = rho4 - .5*rhoavg*dvm/aavg + dv0
      mom2 = mom1 - .5*rhoavg*lambdap*dvp/aavg
c     mom2 = mom4 - .5*rhoavg*lambdam*dvm/aavg + uavg*dv0
      re2  = re1 - .5*rhoavg*(havg+aavg*uavg)*dvp/aavg
c     re2  = re4 - .5*rhoavg*(havg-aavg*uavg)*dvm/aavg
c     re2  = re2 - .5*uavg*uavg*dv0
      u2 = mom2/rho2
      p2 = (gamma-1.)*(re2-.5*mom3*u2)
      ent2 = cu*log(p2)-cp*log(rho2)
      a2 = sqrt(gamma*p2/rho2)
      h2 = (re2+p2)/rho2

      open ( unit=12, file='waves.out' )
      write(12,*) '   To left of - wave:'
      write(12,*) 'pressure= ',p4
      write(12,*) 'density= ', rho4
      write(12,*) 'velocity= ', u4
      write(12,*) ' '
      write(12,*) '   - WAVE'
      write(12,*) 'strength= ', dvm
      write(12,*) 'speed= ', lambdam
      write(12,*) 'location= ', lambdam*t
      write(12,*) ' '
      write(12,*) '   Between - and 0 waves:'
      write(12,*) 'pressure= ',p3
      write(12,*) 'density= ', rho3
      write(12,*) 'velocity= ', u3
      write(12,*) ' '
      write(12,*) '   0 WAVE'
      write(12,*) 'strength= ', dv0
      write(12,*) 'speed= ', lambda0
      write(12,*) 'location= ', lambda0*t
      write(12,*) ' '
      write(12,*) '   Between 0 and + waves:'
      write(12,*) 'pressure= ', p2
      write(12,*) 'density= ', rho2
      write(12,*) 'velocity= ', u2
      write(12,*) ' '
      write(12,*) '   + WAVE'
      write(12,*) 'strength= ', dvp
      write(12,*) 'speed= ', lambdap
      write(12,*) 'location= ', lambdap*t
      write(12,*) ' '
      write(12,*) '   To right of + waves:'
      write(12,*) 'pressure= ', p1
      write(12,*) 'density= ', rho1
      write(12,*) 'velocity= ', u1
      close(unit=12)

      open ( unit = 13, file = 'pressure.out')
      open ( unit = 14, file = 'velocity.out')
      open ( unit = 15, file = 'sound.speed')
      open ( unit = 16, file = 'density.out')
      open ( unit = 17, file = 'entropy.out')
      open ( unit = 18, file = 'mach.out')

      do 30, i=1,N+1
        x = aa + (bb-aa)*real(i-1)/real(N)
        if(x.lt.lambdam*t) then
            write(13,*) x, p4
            write(14,*) x, u4
            write(15,*) x, a4
            write(16,*) x, rho4
            write(17,*) x, ent4
            write(18,*) x, u4/a4
        elseif(x.ge.lambdam*t.and.x.lt.lambda0*t) then
            write(13,*) x, p3
            write(14,*) x, u3
            write(15,*) x, a3
            write(16,*) x, rho3
            write(17,*) x, ent3
            write(18,*) x, u3/a3
        elseif(x.ge.lambda0*t.and.x.lt.lambdap*t) then
            write(13,*) x, p2
            write(14,*) x, u2
            write(15,*) x, a2
            write(16,*) x, rho2
            write(17,*) x, ent2
            write(18,*) x, u2/a2
        else
            write(13,*) x, p1
            write(14,*) x, u1
            write(15,*) x, a1
            write(16,*) x, rho1
            write(17,*) x, ent1
            write(18,*) x, u1/a1
        endif
 30   continue

      close(unit=13)
      close(unit=14)
      close(unit=15)
      close(unit=16)

      stop
      end
