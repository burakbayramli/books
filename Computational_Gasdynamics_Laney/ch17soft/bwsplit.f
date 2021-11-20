      program bwsplit
c...Performs Beam-Warming 2nd order upwind.  Using flux splitting,
c...works regardless of wave direction.
      parameter (nmax=600,delta=.000001)
      double precision lambda,u(-2:nmax+2)
      double precision u0(1:nmax+1), fp,fn,f, x
      double precision dfp, dfn, delfp(-2:nmax+1), delfn(-2:nmax+1)
      double precision ap(-2:nmax+1), an(-2:nmax+1)

c...The derivative df of f should be nonnegative.
      f(x)=x
      df(x)=1.
c     f(x)=.5*x*x
c     df(x)=x

c...Split fluxes.  Sum should equal f.  Derivative dfp of fp should
c...be positive.  Derivative dfn of fn should be negative.
      fp(x)=x
      fn(x)=0.
      dfp(x)=1
      dfn(x)=0.
c     fp(x)=.5*max(0,x)*x
c     fn(x)=.5*min(0,x)*x     
c     dfp(x)=max(0,x)
c     dfn(x)=min(0,x)

      open(unit=9,file='bwsplit.out')

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

      do 100, i=-2,n+1
        delfp(i) = fp(u(i+1))-fp(u(i))
        delfn(i) = fn(u(i+1))-fn(u(i))
        if(abs(u(i+1)-u(i)).gt.delta) then
          ap(i) = delfp(i)/(u(i+1)-u(i))
          an(i) = delfn(i)/(u(i+1)-u(i))
        else
          ap(i) = dfp(u(i))
          an(i) = dfn(u(i))
        endif
 100  continue

      do 110, i=1,n
        u(i) = u(i) - 1.5*lambda*delfp(i-1) + .5*lambda*delfp(i-2)
        u(i) = u(i) - 1.5*lambda*delfn(i) +.5*lambda*delfn(i+1)
        u(i) = u(i) + .5*lambda*lambda*ap(i-1)*delfp(i-1)
        u(i) = u(i) - .5*lambda*lambda*ap(i-2)*delfp(i-2)
        u(i) = u(i) + .5*lambda*lambda*an(i+1)*delfn(i+1)
        u(i) = u(i) - .5*lambda*lambda*an(i)*delfn(i)
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
      open(unit=10,file='bwsplit.plt')
c     write(10,*) 'Beam-Warming Method'
c     write(10,*)  n, lambda, tfinal
      do 900, i=1,n+1
        write(10,1150) -1 + real(2*i-2)/real(n), u(i)
 900  continue
      close(unit=10)

      stop

 1000 write(9,*) '****Error reading data point number ', i,'****'
      close(unit=8)
      close(unit=9)
      stop

 1050 format(4x,'N',11x,'INITIAL',12x,'B-W',11x, 'DIFFERENCE')
 1100 format(i5,5x,f13.8,5x,f13.8,5x,f13.8)
 1150 format(f14.9,5x,f14.9,5x,f14.9)

      end
