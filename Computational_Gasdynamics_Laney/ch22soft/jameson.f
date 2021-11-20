      program jameson
c...Performs Jameson's method.
      parameter (m=2,nmax=800,d=0.00001)
      real lambda, u(-2:nmax+2),h(0:nmax),u0(1:nmax+1)
      real rk(m,m),r(nmax,m),uu(-2:nmax+2)

c...Define the Runge-Kutta coefficients
      if(m.eq.1) then
c...Forward-Euler
        rk(1,1) = 1.
      elseif(m.eq.2) then
c...Improved Euler
        rk(1,1) = 1.
        rk(1,2) = .5
        rk(2,2) = .5
      elseif(m.eq.3) then
c...Heun's 3rd order formula
        rk(1,1) = 1./3.
        rk(1,2) = 0.
        rk(2,2) = 2./3.
        rk(1,3) = .25
        rk(2,3) = .0
        rk(3,3) = .75
      elseif(m.eq.4) then
c..."The" Runge-Kutta method
        rk(1,1) = .5
        rk(1,2) = .0
        rk(2,2) = .5
        rk(1,3) = .0
        rk(2,3) = .0
        rk(3,3) = 1.
        rk(1,4) = 1./6.
        rk(2,4) = 1./3.
        rk(3,4) = 1./3.
        rk(4,4) = 1./6.
      else
        write(*,*) 'Set m between 1 and 4'
        stop
      endif
       
      open(unit=9,file='jameson.out')

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

c...Find the first stage in the Runge-Kutta method.

      call spatial(1,n,u,h)

      do 60, j=1,n
        r(j,1) = lambda*(-h(j)+h(j-1))
        uu(j)  = u(j) + rk(1,1)*r(j,1)
 60   continue

c...Enforce periodicity
      uu(0) = uu(n)
      uu(-1) = uu(n-1)
      uu(-2) = uu(n-2)
      uu(n+1) = uu(1)
      uu(n+2) = uu(2)

c...Find the subsquent stages in the Runge-Kutta method
      do 120, i=2,m
        call spatial(1,n,uu,h)
        do 90, j=1,n
          r(j,i) = lambda*(-h(j)+h(j-1))
          uu(j) = u(j)
          do 90, k=1,i
            uu(j) = uu(j) + rk(k,i)*r(j,k)
 90       continue

c...Enforce periodicity
          uu(0) = uu(n)
          uu(-1) = uu(n-1)
          uu(-2) = uu(n-2)
          uu(n+1) = uu(1)
          uu(n+2) = uu(2)

 120    continue

c...Update the solution
        do 130, j=1,n
          u(j) = uu(j)
 130    continue

c...Enforce periodicity
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
      open(unit=10,file='jameson.plt')
      do 900, i=1,n+1
        write(10,1150) -1 + real(2*i-2)/real(n), u(i)
 900  continue
      close(unit=10)

      stop

 1000 write(9,*) '****Error reading data point number ', i,'****'
      close(unit=8)
      close(unit=9)
      stop

 1050 format(4x,'N',11x,'INITIAL',12x,'JAM',11x, 'DIFFERENCE')
 1100 format(i5,5x,f13.8,5x,f13.8,5x,f13.8)
 1150 format(f14.9,5x,f14.9,5x,f14.9)

      end

      subroutine spatial (ncall,n,u,h)

      parameter (nmax=800,d=0.00001)
      real u(-2:nmax+2),h(0:nmax),a(0:nmax)
      real h1(0:nmax),h2(0:nmax)
      real theta(0:nmax),theta1(-1:nmax+1)
      real kappa, delta, delta2

      save theta

c     f(x)=.5*x*x
c     df(x)=x
      f(x)= x
      df(x)= 1.

c     kappa = .5
c     delta = .002

c     kappa = 0.
c     delta = 0.

      kappa = 1.d0
      delta = .25 d0
      delta2 = 0.00001

      if(ncall.eq.1) then

c...Determine the convex linear combination parameter theta.
        do 30, i=-1,n+1
          theta1(i) = abs(u(i+1)-2.*u(i)+u(i-1))
c...Original denominator
c         temp = abs(u(i+1) + 2.*u(i) + u(i-1))
c...Improved denominator (prevents over-large theta).
          temp = abs(u(i+1)) + 2.*abs(u(i)) + abs(u(i-1))
          if(temp.gt.delta2) then
             theta1(i) = theta1(i)/temp
          else
            theta1(i) = 0.
          endif
 30     continue

        do 35, i=0,n
          theta(i) = kappa*max(theta1(i),theta1(i+1))
c...Unecessary w/improved denominator
          theta(i) = min(theta(i),1.)
 35     continue

      endif

c...Compute Roe-average wave speed
      do 40, i=0,n
        if(abs(u(i+1)-u(i)).gt.d) then
         a(i) = (f(u(i+1))-f(u(i)))/(u(i+1)-u(i))
        else
         a(i) = df(u(i))
        endif
 40   continue

c...Compute a different average
c     do 40, i=0,n
c       a(i) = df( .5*(u(i+1)+u(i)) )
c40   continue

      do 50, i=0,n
c...Calculate 2nd order a.v. of Roe's method
        h1(i) = -theta(i)*abs(a(i))*(u(i+1)-u(i))
 50   continue

      do 110, i=0,n
c...Calculate central differences plus fourth-order a.v.
c       h2(i) = u(i+2)-3.*u(i+1)+3.*u(i)-u(i-1)
        h2(i) = abs(a(i))*(u(i+2)-3.*u(i+1)+3.*u(i)-u(i-1))
        h2(i) = max( 0. , delta - theta(i))*h2(i)
 110  continue

c...Blend two methods.
      do 115, i=0,n
        h(i) = .5*( f(u(i+1))+f(u(i))+h1(i)+h2(i) )
 115  continue
       
      return

      end
