      program eno2
c...Performs 2nd-order ENO as in Shu and Osher:
c           time discretization---pt.1 (Heun's method)
c           spatial discretization---pt.2, algorithm 2.1
      parameter (nmax=100)
      real lambda,u(-2:nmax+2),u0(nmax+1),u1(-2:nmax+2)
      real l0(nmax),l1(nmax),first(-1:nmax)

c...The derivative df of the flux function f should be strictly
c...positive!
      f(x)=x
      df(x)=1.

      open(unit=9,file='eno2.out')

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

      call spatial(n,u,first,l0,1)

      do 100, i=1,n
        u1(i) = u(i) + lambda*l0(i)
 100  continue

      u1(0) = u1(n)
      u1(-1) = u1(n-1)
      u1(-2) = u1(n-2)
      u1(n+1) = u1(1)
      u1(n+2) = u1(2)

      call spatial(n,u1,first,l1,2)

      do 110, i=1,n
        u(i) = u(i) + .5*lambda*l0(i) + .5*lambda*l1(i)
 110  continue

      u(0) = u(n)
      u(-1) = u(n-1)
      u(-2) = u(n-2)
      u(n+1) = u(1)
      u(n+2) = u(2)
 500  continue

      sum = 0.
      write(9,*)
      write(9,1050)
      do 800, i=1,n+1
        sum = sum + abs(u(i)-u0(i))
        write(9,1100) i, u0(i), u(i), abs(u(i)-u0(i))
 800  continue
       write(9,*) 'L1 ERROR = ', sum/real(n+1)

      close(unit=8)
      close(unit=9)

C...write simple file for plotting
      open(unit=10,file='eno2.plt')
      do 900, i=1,n+1
        write(10,1150) -1 + real(2*i-2)/real(n), u(i)
 900  continue
      close(unit=10)

      stop

 1000 write(9,*) '****Error reading data point number ', i,'****'
      close(unit=8)
      close(unit=9)
      stop

 1050 format(4x,'N',11x,'INITIAL',12x,'ENO-2',10x, 'DIFFERENCE')
 1100 format(i5,5x,f13.8,5x,f13.8,5x,f13.8)
 1150 format(f14.9,5x,f14.9)

      end


      subroutine spatial(n,u,first,l,ncall)
c...Calculate second-order spatial discretization.
c...Use linear interpolation.
      parameter(nmax=100)
      real u(-2:n+2),l(n),first(-1:n)
      real flux(-1:nmax+1),h(0:nmax)

      f(x)=x

      do 10, i=-1,n+1
        flux(i)   = f(u(i))
 10   continue

      if(ncall.eq.1) then
      do 15, i=-1,n
        first(i) = flux(i+1)-flux(i)
 15   continue
      endif

      do 20, i=0,n
        if(abs(first(i-1)).lt.abs(first(i))) then
          h(i) = 1.5*flux(i)-.5*flux(i-1)
        else
          h(i) = .5*flux(i+1)+.5*flux(i)
        endif
 20   continue

      do 30, i=1,n
        l(i) = -h(i)+h(i-1)
 30   continue

      return
      end
