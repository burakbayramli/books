      program uno
c...Performs the Uniformly Non-Oscillatory Method (UNO) described in
c...Harten and Osher, SIAM J. Numer. Anal., vol. 24, p. 279-309 (1987).
      parameter (nmax=650,delta=.000001)
      real lambda,u(-2:nmax+2),h(0:nmax),u0(1:nmax+1),S(0:nmax+1)
      real delta_u(-2:nmax+1),delta2_u(-1:nmax+1),a(-1:nmax+1)
      real minmod

c     f(x) =  .5*x*x
c     df(x) =  x

      f(x) = x
      df(x)= 1.

      open(unit=9,file='uno.out')

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

      do 50, i=-2,n+1
        delta_u(i) = u(i+1)-u(i)
 50   continue

      do 60, i=-1,n+1
        delta2_u(i) = .5*(delta_u(i)-delta_u(i-1))
 60   continue

      do 70, i=-1,n+1
        if(abs(delta_u(i)).gt.delta) then
          a(i) = lambda*(f(u(i+1))-f(u(i)))/delta_u(i)
        else
          a(i) = lambda*df(u(i))
        endif
 70   continue

      do 80, i=0,n+1
        t1 = minmod(delta2_u(i),delta2_u(i+1))
        t2 = minmod(delta2_u(i-1),delta2_u(i))
        S(i) = minmod(delta_u(i)-t1,delta_u(i-1)+t2)
 80   continue

      do 90, i=0,n
        if(a(i).ge.0) then
      h(i)=lambda*f(u(i))+.5*a(i)*(1.-a(i-1))*S(i)/(1.+a(i)-a(i-1))
        else
      h(i)=lambda*f(u(i+1))-.5*a(i)*(1.+a(i+1))*S(i+1)/(1.+a(i+1)-a(i))
        endif
 90   continue    

      do 100, i=1,n
        u(i) = u(i)-h(i)+h(i-1)
 100  continue

      u(0) = u(n)
      u(-1) = u(n-1)
      u(-2) = u(n-2)
      u(n+1) = u(1)
      u(n+2) = u(2)
 500  continue

      sum = 0.
      smax = 0.
      write(9,*)
      write(9,1050)
      do 800, i=1,n+1
	sum = sum + abs(u(i)-u0(i))
	smax = max(smax,abs(u(i)-u0(i)))
        write(9,1100) i, u0(i), u(i), abs(u(i)-u0(i))
 800  continue
      write(9,*) 'L1 ERROR = ', sum/real(n+1)
      write(9,*) 'MAX ERROR = ', smax

      close(unit=8)
      close(unit=9)

C...write simple file for plotting
      open(unit=10,file='uno.plt')
c     write(10,*) 'UNO'
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

 1050 format(4x,'N',11x,'INITIAL',12x,'UNO',11x, 'DIFFERENCE')
 1100 format(i5,5x,f13.8,5x,f13.8,5x,f13.8)
 1150 format(f14.9,5x,f14.9)

      end


      real function minmod(x,y)

      if(x*y.lt.0.) then
        minmod = 0.
      elseif(abs(x).ge.abs(y)) then
        minmod = y
      else
        minmod = x
      endif
       
      return
      end
