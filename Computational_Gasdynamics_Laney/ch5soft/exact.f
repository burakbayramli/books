      program exact
c...Find the exact solution to the Riemann problem for the
c...Euler equations.
      real*8 gamma, p1, p2, p3, p4, u1, u2, u3, u4
      real*8 a1, a2, a3, a4, p, aa, bb, t, x, y, fg
      real*8 du, fx, fy, u, a, rho1, rho2, rho3, rho4
      real*8 tol, g1, g2, g3, t1, t2, t3, R, cu, cp
      real*8 s1, s2, s3, s4, diff1, diff2, s, maxa
      real*8 ent, ent1, ent2, ent3, ent4, rho, z, fz
      logical revflag, converge

      parameter(gamma=1.4,g1=.5*(gamma-1.)/gamma,g2=.5*(gamma+1.)/gamma)
      parameter(g3=(gamma+1.)/(gamma-1.), N = 2000)
      parameter(tol=1.E-10)
      parameter (R = 287.0, cu = R/(gamma-1), cp = gamma*R/(gamma-1.))

      fg(x) = (x-1.) / sqrt(g2*(x-1.)+1.)

      open ( unit=11, file='input.dat' )
      read( 11, *) aa, bb, t
      read( 11, *) p4, rho4, u4
      read( 11, *) p1, rho1, u1
      close(unit=11)

c...If necessary, switch states 1 and 4 so that high pressure
c...is on left and low pressure on right
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

      maxa = max(abs(u1+a1),abs(u1-a1),abs(u4-a4),abs(u4+a4))
      write(*,*) 'initial maximum wave speed =',  maxa
        
      p = (p4/p1)**g1
      du = u4 - u1

c...Apply the secant method
      x = .05*p4/p1
      y = .5*p4/p1
      fx = p - (x**g1) / (1.+g1*(gamma*du-a1*fg(x))/a4 )
      fy = p - (y**g1) / (1.+g1*(gamma*du-a1*fg(y))/a4 )
      converge = .false.

      maxa = 0.

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
        write (*, *) i
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

c...Compute entropy
      ent1 = cu*log(p1)-cp*log(rho1)
      ent2 = cu*log(p2)-cp*log(rho2)
      ent3 = cu*log(p3)-cp*log(rho3)
      ent4 = cu*log(p4)-cp*log(rho4)

      open ( unit=12,file='waves.out' )

      if(revflag) then
        write(12,*) '   SHOCK'
        write(12,*) 'speed= ', -s1
        write(12,*) 'location= ', -s1*t
        write(12,*) '   CONTACT'
        write(12,*) 'speed= ', -s2, -s2*t
        write(12,*) 'location= ', -s2*t
        write(12,*) '   EXPANSION'
        write(12,*) 'left speed= ', -s3
        write(12,*) 'left endpoint= ', -s3*t
        write(12,*) 'right speed= ', -s4
        write(12,*) 'right endpoint = ', -s4*t
      else
        write(12,*) '   To left of expansion'
        write(12,*) 'pressure= ', p4
        write(12,*) 'density= ', rho4
        write(12,*) 'velocity= ', u4
        write(12,*) ' '
        write(12,*) '   EXPANSION'
        s = (rho3*u3 - rho4*u4)/(rho3-rho4)
        diff1 = ent3 - ent4
        diff2 = a3-a4+.5*(gamma-1.)*(u3-u4)
        if(s.le.s3.and.s.ge.s4.and.abs(diff1).lt..001.and.
     .                            abs(diff2).lt..001) then
          write(12,*) 'expansion checks'
          write(12,*) s
        else
          write(12,*) 'expansion is wrong'
          write(12,*) s, diff1, diff2
        endif
        write(12,*) 'left speed= ', s4
        write(12,*) 'left endpoint= ', s4*t
        write(12,*) 'right speed= ', s3
        write(12,*) 'right endpoint = ', s3*t
        write(12,*) ' '
        write(12,*) '   Between expansion and contact'
        write(12,*) 'pressure= ', p3
        write(12,*) 'density= ', rho3
        write(12,*) 'velocity= ', u3
        write(12,*) ' '
        write(12,*) '   CONTACT'
        s = (rho3*u3 - rho2*u2)/(rho3-rho2)
        if (abs(s-s2).lt..001.and.abs(p2-p3).lt..001.and.
     .                           abs(u2-u3).lt..001) then
          write(12,*) 'contact satisfies Rankine-Hugoniot'
        else
          write(12,*) 'contact violates Rankine-Hugoniot'
          write(12,*) s
        endif
        write(12,*) 'speed= ', s2
        write(12,*) 'location= ', s2*t
        write(12,*) ' '
        write(12,*) '   Between contact and shock'
        write(12,*) 'pressure= ', p2
        write(12,*) 'density= ', rho2
        write(12,*) 'velocity= ', u2
        write(12,*) ' '
        write(12,*) '   SHOCK'
        s = (rho1*u1 - rho2*u2)/(rho1-rho2)
        diff1 = rho1*(u1-s)*(u1-s)-rho2*(u2-s)*(u2-s)+p1-p2
        diff2 = gamma*p1/((gamma-1.)*rho1)+.5*(u1-s)*(u1-s)
        diff2 = diff2-gamma*p2/((gamma-1.)*rho2)-.5*(u2-s)*(u2-s)
        if(abs(s-s1).lt..001.and.abs(diff1).lt..001.and.
     .                           abs(diff2).lt..001) then
          write(12,*) 'shock satisfies Rankine-Hugoniot'
        else
          write(12,*) 'shock violates Rankine-Hugoniot'
          write(12,*) s,diff1,diff2
        endif
        if(ent1.le.ent2.and.u2+a2.ge.s1.and.s1.ge.u1+a1) then
          write(12,*) 'shock satisfies 2nd law'
        else
          write(12,*) 'shock violates 2nd law'
          write(12,*) u2+a2, u1+a1
        endif
        write(12,*) 'speed= ', s1
        write(12,*) 'location= ', s1*t
        write(12,*) ' '
        write(12,*) '   To right of shock'
        write(12,*) 'pressure= ', p1
        write(12,*) 'density= ', rho1
        write(12,*) 'velocity= ', u1
      endif
      close(unit=12)

      open ( unit = 13, file = 'pressure.out')
      open ( unit = 14, file = 'velocity.out')
      open ( unit = 15, file = 'sound.speed')
      open ( unit = 16, file = 'density.out')
      open ( unit = 17, file = 'entropy.out')
      open ( unit = 18, file = 'mach.out')
      open ( unit = 19, file = 'massflux.out')

      do 30, i=1,N+1
        x = aa + (bb-aa)*real(i-1)/real(N)
        if(revflag) then
          if(x.ge.-s4*t) then
            write(13,*) x, p4
            write(14,*) x, -u4
            write(15,*) x, a4
            write(16,*) x, rho4
            write(17,*) x, ent4
            write(18,*) x, -u4/a4
            write(19,*) x, -rho4*u4
            maxa = max (maxa, abs(-u4+a4),abs(-u4-a4))
            if(abs(-u4+a4).lt.0.1.or.abs(-u4-a4).lt.0.1) then
              write(*,*) 'sonic point at ', x 
            endif
          elseif(x.lt.-s4*t.and.x.ge.-s3*t) then
            u = 2.*( -(x/t)-.5*(gamma-1.)*u4 +a4 )/(gamma+1.)
            a = u + (x/t)
            p = p4*(a/a4)**(2.*gamma/(gamma-1.))
            write(13,*) x, p
            write(14,*) x, -u
            write(15,*) x, a
            rho = gamma*p/(a*a)
            write(16,*) x, rho
            ent = cu*log(p)-cp*log(rho)
            write(17,*) x, ent
            write(18,*) x, -u/a
            write(19,*) x, -rho*u
            maxa = max (maxa, abs(-u+a),abs(-u-a))
            if(abs(-u+a).lt.0.1.or.abs(-u-a).lt.0.1) then
              write(*,*) 'sonic point at ', x 
            endif
          elseif(x.lt.-s3*t.and.x.ge.-s2*t) then
            write(13,*) x, p3
            write(14,*) x, -u3
            write(15,*) x, a3
            write(16,*) x, rho3
            write(17,*) x, ent3
            write(18,*) x, -u3/a3
            write(19,*) x, -rho3*u3
            maxa = max (maxa, abs(-u3+a3),abs(-u3-a3))
            if(abs(-u3+a3).lt.0.1.or.abs(-u3-a3).lt.0.1) then
              write(*,*) 'sonic point at ', x 
            endif
          elseif(x.lt.-s2*t.and.x.ge.-s1*t) then
            write(13,*) x, p2
            write(14,*) x, -u2
            write(15,*) x, a2
            write(16,*) x, rho2
            write(17,*) x, ent2
            write(18,*) x, -u2/a2
            write(19,*) x, -rho2*u2
            maxa = max (maxa, abs(-u2+a2),abs(-u2-a2))
            if(abs(-u2+a2).lt.0.1.or.abs(-u2-a2).lt.0.1) then
              write(*,*) 'sonic point at ', x 
            endif
          else
            write(13,*) x, p1
            write(14,*) x, -u1
            write(15,*) x, a1
            write(16,*) x, rho1
            write(17,*) x, ent1
            write(18,*) x, -u1/a1
            write(19,*) x, -rho1*u1
            maxa = max (maxa, abs(-u1+a1),abs(-u1-a1))
            if(abs(-u1+a1).lt.0.1.or.abs(-u1-a1).lt.0.1) then
              write(*,*) 'sonic point at ', x 
            endif
          endif
        else
          if(x.lt.s4*t) then
            write(13,*) x, p4
            write(14,*) x, u4
            write(15,*) x, a4
            write(16,*) x, rho4
            write(17,*) x, ent4
            write(18,*) x, u4/a4
            write(19,*) x, rho4*u4
            maxa = max (maxa, abs(u4+a4),abs(u4-a4))
            if(abs(u4+a4).lt.0.1.or.abs(u4-a4).lt.0.1) then
              write(*,*) 'sonic point at ', x 
            endif
          elseif(x.ge.s4*t.and.x.lt.s3*t) then
            u = 2.*( (x/t) + .5*(gamma-1.)*u4 +a4 )/(gamma+1.)
            a = u - (x/t)
            p = p4*(a/a4)**(2.*gamma/(gamma-1.))
            write(13,*) x, p
            write(14,*) x, u
            write(15,*) x, a
            rho = gamma*p/(a*a)
            write(16,*) x, rho
            ent = cu*log(p)-cp*log(rho)
            write(17,*) x, ent
            write(18,*) x, u/a
            write(19,*) x, rho*u
            maxa = max (maxa, abs(u+a),abs(u-a))
            if(abs(u+a).lt.0.1.or.abs(u-a).lt.0.1) then
              write(*,*) 'sonic point at ', x 
            endif
          elseif(x.ge.s3*t.and.x.lt.s2*t) then
            write(13,*) x, p3
            write(14,*) x, u3
            write(15,*) x, a3
            write(16,*) x, rho3
            write(17,*) x, ent3
            write(18,*) x, u3/a3
            write(19,*) x, rho3*u3
            maxa = max (maxa, abs(u3+a3),abs(u3-a3))
            if(abs(u3+a3).lt.0.1.or.abs(u3-a3).lt.0.1) then
              write(*,*) 'sonic point at ', x 
            endif
          elseif(x.ge.s2*t.and.x.lt.s1*t) then
            write(13,*) x, p2
            write(14,*) x, u2
            write(15,*) x, a2
            write(16,*) x, rho2
            write(17,*) x, ent2
            write(18,*) x, u2/a2
            write(19,*) x, rho2*u2
            maxa = max (maxa, abs(u2+a2),abs(u2-a2))
            if(abs(u2+a2).lt.0.1.or.abs(u2-a2).lt.0.1) then
              write(*,*) 'sonic point at ', x 
            endif
          else
            write(13,*) x, p1
            write(14,*) x, u1
            write(15,*) x, a1
            write(16,*) x, rho1
            write(17,*) x, ent1
            write(18,*) x, u1/a1
            write(19,*) x, rho1*u1
            maxa = max (maxa, abs(u1+a1),abs(u1-a1))
            if(abs(u1+a1).lt.0.1.or.abs(u1-a1).lt.0.1) then
              write(*,*) 'sonic point at ', x 
            endif
           endif
         endif
 30   continue

      write(*,*) 'final maximum wave speed =', maxa

      close(unit=13)
      close(unit=14)
      close(unit=15)
      close(unit=16)
      close(unit=17)
      close(unit=18)
      close(unit=19)

      stop
      end
