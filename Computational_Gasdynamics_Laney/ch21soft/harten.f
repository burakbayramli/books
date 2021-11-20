      program harten 
c...Performs Harten's method
      parameter (nmax=1000,delta=.000001,del=0.5)
      real lambda,u(-2:nmax+2),h(0:nmax),u0(1:nmax+1),a(-1:nmax+1)
      real delf(-1:nmax+1),g(0:nmax+1), eps(-1:nmax+1)

      f(x)=x
      df(x)=1.

c     f(x) = .5*x*x
c     df(x) = x

      open(unit=9,file='harten.out')

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
      if(itert.gt.50000) then
       write(9,*) 'Too many iterations'
       stop
      endif
      write(9,*) 'Final time requested: ', tfinal
      tfinal = real(itert)*delta_t
      write(9,*) 'Actual final time: ', tfinal
      write(9,*) 'delta_t = ', delta_t
      write(9,*) 'delta_x = ', delta_x
      write(9,*) 'lambda = ', lambda

 
      do 500, it=1,itert

      do 100, i=-1,n+1
        if(abs(u(i+1)-u(i)).gt.delta) then
         a(i) = lambda*(f(u(i+1))-f(u(i)))/(u(i+1)-u(i))
        else
         a(i) = lambda*df(u(i))
        endif
        if(abs(a(i)).lt.del) then
          eps(i) = .5*(a(i)*a(i)+del*del)/del
        else
          eps(i) = abs(a(i))
        endif
 100  continue

      do 110, i=-1,n+1
        delf(i)=.5*(eps(i)-a(i)*a(i))*(u(i+1)-u(i))/lambda
 110  continue

      do 120, i=0,n+1
        sn = sign(1.,delf(i))
        temp = min(sn*delf(i),sn*delf(i-1))
        g(i) = f(u(i)) + sn*max(0.,temp)
 120  continue

      do 130, i=0,n
        if(abs(u(i+1)-u(i)).gt.delta) then
         a(i) = (g(i+1)-g(i))/(u(i+1)-u(i))
        else
         a(i) = df(u(i))
        endif
        if(abs(a(i)).lt.del) then
          eps(i) = .5*(a(i)*a(i)+del*del)/del
        else
          eps(i) = abs(a(i))
        endif
 130  continue

      do 140, i=0,n
        h(i) = .5*lambda*(g(i+1)+g(i)-eps(i)*(u(i+1)-u(i)))
 140  continue

      do 150, i=1,n
        u(i) = u(i) - h(i) + h(i-1)
 150  continue

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
      open(unit=10,file='harten.plt')
      do 900, i=1,n+1
        write(10,1150) -1 + real(2*i-2)/real(n), u(i)
 900  continue
      close(unit=10)

      stop

 1000 write(9,*) '****Error reading data point number ', i,'****'
      close(unit=8)
      close(unit=9)
      stop

 1050 format(4x,'N',11x,'INITIAL',11x,'HARTEN',9x, 'DIFFERENCE')
 1100 format(i5,5x,f13.8,5x,f13.8,5x,f13.8)
 1150 format(f14.9,5x,f14.9)

      end
