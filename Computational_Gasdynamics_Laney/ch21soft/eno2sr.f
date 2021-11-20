      program eno2sr
c...Performs 2nd-order ENO as in Shu and Osher:
c           time discretization---pt.1 (Heun's method)
c           spatial discretization---pt.2, algorithm 2.1 + 3.1
      parameter (nmax=100)
      real lambda,u(-2:nmax+2),u0(nmax+1),u1(-2:nmax+2)
      real l0(nmax),l1(nmax),theta(-1:nmax),first(-2:nmax+2)
      logical crit(-1:nmax)

c...The derivative df of the flux function f should be strictly
c...positive!
      f(x)=x
      df(x)=1.

      open(unit=9,file='eno2sr.out')

c...Read initial data samples.  Samples evenly spaced.
c...Data assumed periodic.
      open(unit=8,file='nb.dat',status='old')
      read(8,*) n, lambda, tfinal
      if(n.gt.nmax) then
        write(9,*) '****Too many data points****'
        close(unit=8)
        close(unit=9)
        stop
      endif
      if(n.lt.2) then
        write(9,*) '****Too few data points****'
        close(unit=8)
        close(unit=9)
        stop
      endif
      if(lambda.lt.0.01) then
        write(9,*) '****Lambda small or negative****'
        close(unit=8)
        close(unit=9)
        stop
      endif
      i=1
      read(8,*,err=1000,end=1000) xmin, u(1)
      do 10, i=2,n
        read(8,*,err=1000,end=1000) dummy, u(i)
 10   continue
      i=n+1
      read(8,*,err=1000,end=1000) xmax, u(n+1)
      if(abs(u(n+1)-u(1)).gt..0001) then
        write(9,*) '****Data not periodic****'
        close(unit=8)
        close(unit=9)
        stop
      endif
      if(xmax.le.xmin+.0001) then
        write(9,*) '****Bad x-axis****'
        close(unit=8)
        close(unit=9)
        stop
      endif
      u(0) = u(n)
      u(-1) = u(n-1)
      u(-2) = u(n-2)
      u(n+2) = u(2)
      do 15, i=1,n+1
        u0(i) = u(i)
 15   continue

      delta_x=(xmax-xmin)/real(n)
      delta_t=lambda*delta_x
      itert=nint(tfinal/delta_t)
      write(9,*) 'Final time requested: ', tfinal
      tfinal = real(itert)*delta_t
      write(9,*) 'Actual final time: ', tfinal
      write(9,*) 'delta_t = ', delta_t
      write(9,*) 'delta_x = ', delta_x
      write(9,*) 'lambda = ', lambda


      do 500, it=1,itert

      call spatial(n,u,l0,lambda,first,theta,crit,1)

      do 100, i=1,n
        u1(i) = u(i) + lambda*l0(i)
 100  continue

      u1(0) = u1(n)
      u1(-1) = u1(n-1)
      u1(-2) = u1(n-2)
      u1(n+1) = u1(1)
      u1(n+2) = u1(2)

      call spatial(n,u1,l1,lambda,first,theta,crit,2)

      do 110, i=1,n
        u(i) = u(i) + .5*lambda*l0(i) + .5*lambda*l1(i)
 110  continue

      u(0) = u(n)
      u(-1) = u(n-1)
      u(-2) = u(n-2)
      u(n+1) = u(1)
      u(n+2) = u(2)
 500  continue

      write(9,*)
      write(9,1050)
      do 800, i=1,n+1
        write(9,1100) i, u0(i), u(i), abs(u(i)-u0(i))
 800  continue

      close(unit=8)
      close(unit=9)

C...write simple file for plotting
      open(unit=10,file='eno2sr.plt')
      do 900, i=1,n+1
        write(10,1150) -1 + real(2*i-2)/real(n), u(i)
 900  continue
      close(unit=10)

      stop

 1000 write(9,*) '****Error reading data point number ', i,'****'
      close(unit=8)
      close(unit=9)
      stop

 1050 format(4x,'N',11x,'INITIAL',10x,'ENO-2SR',10x, 'DIFFERENCE')
 1100 format(i5,5x,f13.8,5x,f13.8,5x,f13.8)
 1150 format(f14.9,5x,f14.9)

      end


      subroutine spatial(n,u,l,lambda,first,theta,crit,ncall)
c...Calculate second-order spatial discretization.
c...Use linear interpolation.
c...Use subcell resolution.
      parameter(nmax=100)
      real u(-2:nmax+2),l(nmax),theta(-1:nmax),first(-2:nmax+2)
      real flux(-2:nmax+3),h(0:nmax+1),s(-2:nmax+1)
      real lambda
      logical crit(-1:nmax)

      f(x)=x
      df(x) = 1.

      do 5, i=-2,n+2
        flux(i) = f(u(i))
 5    continue
      flux(n+3) = flux(3)

c...On the first call, determine the location of any discontinuities
c...and the slopes of the solution.
      if(ncall.eq.1) then

        do 10, i=-2,n+1
          first(i)  = u(i+1) - u(i)
 10     continue
        first(n+2) = first(2)

        do 15, i=-1,n+1
          if(first(i)*first(i-1).ge.-.001) then
            s(i) = min( abs(first(i)),abs(first(i-1)) ) 
          else
            s(i) = 0.
          endif
 15     continue
        s(-2)  = s(n-2)

        do 16, i=-1,n
C...Tag "critical intervals"
          crit(i) = s(i).ge.s(i+1).and.s(i).gt.s(i-1)
          if(crit(i)) then
c...Possible discontinuity in cell i.
c...Determine discontinuity's location.
            if(abs(u(i+1)-u(i-1)).gt..00001) then
              theta(i) = first(i)/(u(i+1)-u(i-1))
            else
              sn = sign(1.,u(i+1)-u(i-1))
              theta(i) = sn*first(i)*100000.
            endif
c...Discontinuity considered confirmed if located in cell i (the original
c...paper does not specify what action to take if theta>1 or theta<0).
            crit(i) = crit(i).and.theta(i).ge.0..and.theta(i).le.1.
            if(crit(i)) theta(i)=(1.-theta(i))/(df(u(i))*lambda)
          endif
 16     continue

      endif

      do 20, i=0,n

      if(crit(i)) then

        if(abs(first(i-2)).lt.abs(first(i-1))) then
          hl = 2.5*flux(i-1) - 1.5*flux(i-2)
        else
          hl = 1.5*flux(i)-.5*flux(i-1)
        endif

c       if(ncall.gt.1.and.theta(i).lt.1.) then
c...Discontinuity in cell i may travel to cell i+1.
c         if(abs(first(i+2)).lt.abs(first(i+1))) then
c           hr = -1.5*flux(i+3)+2.5*flux(i+2)
c         else
c           hr = -.5*flux(i+2)+1.5*flux(i+1)
c         endif
c       else
          if(abs(first(i+1)).lt.abs(first(i))) then
            hr = -.5*flux(i+2)+1.5*flux(i+1)
          else
            hr = .5*flux(i+1)+.5*flux(i)
          endif
c       endif

c       if(theta(i).ge.1.) then
c         h(i) = hr
c       else
c         h(i) = hl
c       endif

        theta(i)=min(1.,theta(i))
        h(i) = (1.-theta(i))*hl + theta(i)*hr

c       h(i) = theta(i)*hl + (1.-theta(i))*hr

c     elseif(ncall.gt.1.and.crit(i-1).and.theta(i-1).lt.1.) then
c...Discontinuity in cell i-1 may travel to cell i.  "To be safe..."
c       if(abs(first(i+1)).lt.abs(first(i))) then
c         h(i) = -.5*flux(i+2)+1.5*flux(i+1)
c       else
c         h(i) = .5*flux(i+1)+.5*flux(i)
c       endif
        
      else
c...Proceed as usual.
        if(abs(first(i-1)).lt.abs(first(i))) then
          h(i) = 1.5*flux(i)-.5*flux(i-1)
        else
          h(i) = .5*flux(i+1)+.5*flux(i)
        endif
      endif

 20   continue

      do 30, i=1,n
        l(i) = -h(i)+h(i-1)
 30   continue

 100  format(f13.8,5x,f13.8,5x,f13.3)
      return
      end
