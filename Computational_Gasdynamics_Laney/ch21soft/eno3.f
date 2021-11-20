      program eno3
c...Performs 3rd-order ENO as in Shu and Osher:
c           time discretization---pt.1 
c           spatial discretization---pt.2, algorithm 2.1
      parameter (nmax=100)
      real lambda,u(-2:nmax+2),u0(nmax+1),u1(-2:nmax+2),u2(-2:nmax+2)
      real l0(nmax),l1(nmax),l2(nmax),ufe(-2:nmax+2)

c...The derivative df of the flux function f should be strictly
c...positive!
      f(x)=x
      df(x)=1.

      open(unit=9,file='eno3.out')

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
	ufe(i) = u(i)
 15   continue
      ufe(0) = ufe(n)
      ufe(-1) = ufe(n-1)
      ufe(-2) = ufe(n-2)
      ufe(n+2) = ufe(2)

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

      call spatial(n,u,l0)

      do 100, i=1,n
        u1(i) = u(i) + lambda*l0(i)
 100  continue

      u1(0) = u1(n)
      u1(-1) = u1(n-1)
      u1(-2) = u1(n-2)
      u1(n+1) = u1(1)
      u1(n+2) = u1(2)

      call spatial(n,u1,l1)

      do 110, i=1,n
        u2(i) = .75*u(i)+.25*u1(i)+.25*lambda*l1(i)
 110  continue

      u2(0) = u2(n)
      u2(-1) = u2(n-1)
      u2(-2) = u2(n-2)
      u2(n+1) = u2(1)
      u2(n+2) = u2(2)

      call spatial(n,u2,l2)

      do 120, i=1,n
        u(i)= (u(i)+2.*u2(i)+2.*lambda*l2(i))/3.
 120  continue

      u(0) = u(n)
      u(-1) = u(n-1)
      u(-2) = u(n-2)
      u(n+1) = u(1)
      u(n+2) = u(2)

      call spatial(n,ufe,l0)

      do 130, i=1,n
        ufe(i) = ufe(i) + lambda*l0(i)
 130  continue

      ufe(0) = ufe(n)
      ufe(-1) = ufe(n-1)
      ufe(-2) = ufe(n-2)
      ufe(n+1) = ufe(1)
      ufe(n+2) = ufe(2)

 500  continue

      sum = 0.
      sum1 = 0.
      write(9,*)
      write(9,1050)
      do 800, i=1,n+1
	sum = sum + abs(u(i)-u0(i))
        sum1 = sum1 + abs(ufe(i)-u0(i))
        write(9,1100) i, u0(i), u(i), abs(u(i)-u0(i)), ufe(i)
 800  continue
      write(9,*) 'L1 ERROR (RUNGE-KUTTA) = ', sum/real(n+1)
      write(9,*) 'L1 ERROR (FORWARD-EULER) =', sum1/real(n+1)

      close(unit=8)
      close(unit=9)

C...write simple file for plotting
      open(unit=10,file='eno3.plt')
      do 900, i=1,n+1
        write(10,1150) -1 + real(2*i-2)/real(n), u(i)
 900  continue
      close(unit=10)

      stop

 1000 write(9,*) '****Error reading data point number ', i,'****'
      close(unit=8)
      close(unit=9)
      stop

 1050 format(4x,'N',11x,'INITIAL',11x,'ENO-3',13x,'DIFF',14x,'F.E.')
 1100 format(i5,5x,f13.8,5x,f13.8,5x,f13.8,5x,f13.8)
 1150 format(f14.9,5x,f14.9)

      end


      subroutine spatial(n,u,l)
c...Calculate 3rd-order spatial discretization.
c...Use quadratic interpolation.
      parameter(nmax=100)
      real u(-2:n+2),l(n)
      real flux(-2:nmax+2),h(0:nmax)

      f(x)=x

      do 10, i=-2,n+2
        flux(i)   = f(u(i))
 10   continue

      do 20, i=0,n
        a = abs(flux(i+1)-flux(i))
        b = abs(flux(i)-flux(i-1))
        if(a.gt.b) then
          aa = abs(flux(i+1)-2.*flux(i)+flux(i-1))
          bb = abs(flux(i)-2.*flux(i-1)+flux(i-2))
          if(aa.gt.bb) then
            h(i) = (11.*flux(i)-7.*flux(i-1)+2.*flux(i-2))/6.
          else
            h(i) = (2.*flux(i+1)+5.*flux(i)-flux(i-1))/6.
          endif
        else
          aa = abs(flux(i+2)-2.*flux(i+1)+flux(i))
          bb = abs(flux(i+1)-2.*flux(i)+flux(i-1))
          if(aa.gt.bb) then
            h(i) = (2.*flux(i+1)+5.*flux(i)-flux(i-1))/6.
          else
            h(i) = (-flux(i+2)+5.*flux(i+1)+2.*flux(i))/6.
          endif
        endif
 20   continue

      do 30, i=1,n
        l(i) = -h(i)+h(i-1)
 30   continue

      return
      end
