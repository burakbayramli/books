      program fct
c...Performs two-step flux-corrected transport
c...as in Boris and Book, 1973
      real*8 delta
      parameter (nmax=1000,delta=.000000001)
      real*8 lambda,u(-2:nmax+2),hc(0:nmax),u0(1:nmax+1),a(0:nmax)
      real*8 h1(0:nmax),first(-1:nmax+1),u1(-2:nmax+2),x
      real*8 delta_x, delta_t, f, df, temp

      f(x)= x
      df(x)= 1.

c      f(x)= .5*x*x
c      df(x)= x

      open(unit=9,file='fct.out')

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

      do 50, i=-1,n+1
        first(i) = u(i+1)-u(i)
 50   continue

c...Calculate Lax-Wendroff conservative flux plus dissipation.
      do 100, i=0,n
        if(abs(first(i)).gt.delta) then
         a(i) = (f(u(i+1))-f(u(i)))/first(i)
        else
         a(i) = df(u(i))
        endif
 100  continue

      do 110, i=0,n
        h1(i) = .5*lambda*(f(u(i+1))+f(u(i)))
     c        - (.125+.5*lambda*lambda*a(i)*a(i))*first(i)
 110  continue

      do 112, i=1,n
        u1(i) = u(i) - h1(i) + h1(i-1)
 112  continue
      u1(0) = u1(n)
      u1(-1) = u1(n-1)
      u1(-2) = u1(n-2)
      u1(n+1) = u1(1)
      u1(n+2) = u1(2)
      do 113, i=-1,n+1
        first(i) = u1(i+1)-u1(i)
 113  continue

c...Compute flux correction using minmod of adjacent slopes.
      do 115, i=0,n
        sn = sign(1.,first(i))
        temp = min(sn*first(i-1),.125*sn*first(i),sn*first(i+1))
        hc(i) = sn*max(0.,temp)
 115  continue
       
      do 120, i=1,n
c...Boris and Books original FCT
c...Choose only ONE of the equivalent expressions!
c       u(i) = u1(i) - hc(i) + hc(i-1)
        u(i) = u(i) - h1(i)-hc(i) + h1(i-1)+hc(i-1)
c...Boris and Book's first-order upwind method
c       u(i) = u(i) - h1(i)+ h1(i-1)
 120  continue

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
      open(unit=10,file='fct.plt')
c     write(10,*) 'Flux-Corrected Transport'
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

 1050 format(4x,'N',11x,'INITIAL',12x,'FCT',11x, 'DIFFERENCE')
 1100 format(i5,5x,f13.8,5x,f13.8,5x,f13.8)
 1150 format(f14.9,5x,f14.9,5x,f14.9)

      end
