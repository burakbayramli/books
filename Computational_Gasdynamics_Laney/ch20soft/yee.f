      program yee

c...Implicit or explicit

c...For and scalar conservation law, performs the method described
c...in Yee, Construction of Explict and Implicit Symmetric TVD
c...Schemes and Their Applications, J. Comput. Phys., 68, 151 (1987)

      real*8 delta,dhartp
      parameter (nmax=1000,delta=.00000001,dhartp=0.6)
      real*8 delta_t, delta_x
      real*8 lambda,u(-2:nmax+3),h(1:nmax),u0(1:nmax+1)
      real*8 x,f,df, rp(-2:nmax+2), rm(-2:nmax+2),epsilon(-2:nmax+2)
      real*8 aavg(-2:nmax+2),t1,t2,t3,t4, phi(-2:nmax+2)
      real*8 a(1:nmax),b(1:nmax),c(1:nmax),d(1:nmax),theta

c      f(x) = .5*x*x
c      df(x) = x

      f(x) = x
      df(x) = 1.

 2    write(*,*) 'which flux limiter?'
      write(*,*) '1 = van leer'
      write(*,*) '2 = superbee'
      write(*,*) '3 = minmod'
      write(*,*) '4 = combined superbee'
      write(*,*) '5 = combined minmod'
      read(*,*) ifl

      if(ifl.lt.1.or.ifl.gt.5) go to 2

 3    write(*,*) 'implicit coefficient?'
      read(*,*) theta

      if(theta.lt.0..or.theta.gt.1.) go to 3

      open(unit=9,file='yee.out')

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
      u(n+3) = u(3)
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

      do 90, i=-1,n+2

        if(abs(u(i+1)-u(i)).gt.delta) then
          rp(i) = (u(i)-u(i-1))/(u(i+1)-u(i))
          rm(i) = (u(i+2)-u(i+1))/(u(i+1)-u(i))
          aavg(i) = (f(u(i+1))-f(u(i)))/(u(i+1)-u(i))
        else
          rp(i) = sign(1.,(u(i)-u(i-1))*(u(i+1)-u(i)))/delta
          rm(i) = sign(1.,(u(i+2)-u(i+1))*(u(i+1)-u(i)))/delta
          aavg(i) = df(u(i))
        endif

c...Choose Roe's first-order upwind method for use at sonic points.

c       epsilon(i) = abs(aavg(i))

c...Choose Harten's first-order upwind method for use at sonic points.
        if(abs(aavg(i)).gt.dhartp) then
          epsilon(i) = abs(aavg(i))
        else
          epsilon(i) = .5*(aavg(i)*aavg(i)+dhartp*dhartp)/dhartp
        endif

 90   continue

      do 95, i=0,n+1

c       if(abs(aavg(i)).gt.delta) then
c         rp(i) = rp(i)*abs(aavg(i-1)/aavg(i))
c         rm(i) = rm(i)*abs(aavg(i+1)/aavg(i))
c       else
c         rp(i) = rp(i)*abs(aavg(i-1))/delta
c         rm(i) = rm(i)*abs(aavg(i+1))/delta
c       endif

        if(ifl.eq.1) then
c...van Leer
          phi(i) = (rm(i)+abs(rm(i)))/(1.+rm(i))
     *            +(rp(i)+abs(rp(i)))/(1.+rp(i)) -1.
        elseif(ifl.eq.2) then
c...Superbee
          t1 = min(2.*rp(i),1.)
          t2 = min(2.*rm(i),1.)
          t3 = min(rp(i),2.)
          t4 = min(rm(i),2.)
          phi(i) = max(0.,t1,t3)+max(0.,t2,t4)-1.
        elseif(ifl.eq.3) then
c...Minmod
          t1 = min(rp(i),1.)
          t2 = min(rm(i),1.)
          phi(i) = max(0.,t1)+max(0.,t2)-1.
        elseif(ifl.eq.4) then
c...Combined superbee
          t1=min(2.,2.*rp(i),2.*rm(i),.5*(rp(i)+rm(i)))
          phi(i) = max(0.,t1)
        elseif(ifl.eq.5) then
c...Combined minmod
          t1=min(1.,rp(i),rm(i))
          phi(i) = max(0.,t1)
        endif

 95   continue

      do 100, i=1,n
        t1 = epsilon(i)*(phi(i)-1.)
        t2 = epsilon(i-1)*(phi(i-1)-1.)
c...Right hand side 
        d(i) = f(u(i+1))-f(u(i-1))+t1*(u(i+1)-u(i))-t2*(u(i)-u(i-1))
        d(i) = -.5*lambda*d(i)
c...Lower sub-diagonal
        a(i) = .5*lambda*theta*(-df(u(i-1))+t2)
c...Diagonal
        b(i) = 1.0 -.5*lambda*theta*(t1+t2)
c...Upper sub-diagonal
        c(i) = .5*lambda*theta*(df(u(i+1))+t1) 
 100  continue

      call ptrid(n,a,b,c,d,a(n),c(1),h)

      do 120, i=1,n
        u(i) = u(i)+h(i)
 120  continue

      u(0) = u(n)
      u(-1) = u(n-1)
      u(-2) = u(n-2)
      u(n+1) = u(1)
      u(n+2) = u(2)
      u(n+3) = u(3)
 500  continue

      write(9,*)
      write(9,1050)
      do 800, i=1,n+1
        write(9,1100) i, u0(i), u(i), abs(u(i)-u0(i))
 800  continue

      close(unit=8)
      close(unit=9)

C...write simple file for plotting
      open(unit=10,file='yee.plt')
      do 900, i=1,n+1
        write(10,1150) -1 + real(2*i-2)/real(n), u(i)
 900  continue
      close(unit=10)

      stop

 1000 write(9,*) '****Error reading data point number ', i,'****'
      close(unit=8)
      close(unit=9)
      stop

 1050 format(4x,'N',11x,'INITIAL',11x,'YEE',12x, 'DIFFERENCE')
 1100 format(i5,5x,f13.8,5x,f13.8,5x,f13.8)
 1150 format(f14.9,5x,f14.9)

      end

      subroutine ptrid(n,a,b,c,d,e,f,x)
      integer n
      real*8 a(n),b(n),c(n),d(n),e,f,x(n)
      real*8 r(n),s(n)

      r(1) = e
      s(1) = f

c...Zero out the subdiagonal
      do 10, i=2,n-2
c       a(i) = a(i) - a(i)*b(i-1)/b(i-1)
        b(i) = b(i) - a(i)*c(i-1)/b(i-1)
        d(i) = d(i) - a(i)*d(i-1)/b(i-1)
        r(i) =      - a(i)*r(i-1)/b(i-1)
 10   continue

c     a(n-1) = a(n-1) - a(n-1)*b(n-2)/b(n-2)
      b(n-1) = b(n-1) - a(n-1)*c(n-2)/b(n-2)
      c(n-1) = c(n-1) - a(n-1)*r(n-2)/b(n-2)
      d(n-1) = d(n-1) - a(n-1)*d(n-2)/b(n-2)

c     a(n) = a(n) - a(n)*b(n-1)/b(n-1)
      b(n) = b(n) - a(n)*c(n-1)/b(n-1)
      d(n) = d(n) - a(n)*d(n-1)/b(n-1)

      if(abs(f).gt..0001) then
c...Chase the non-zero element from left to right across the
c...bottom row.
        do 15, i=1,n-2
c         s(i)   =  s(i) - s(i)*b(i)/b(i)
          s(i+1) =       - s(i)*c(i)/b(i)
          b(n)   =  b(n) - s(i)*r(i)/b(i)
          d(n)   =  d(n) - s(i)*d(i)/b(i)
 15     continue
c         s(n-1) =  s(n-1) - s(n-1)*b(n-1)/b(n-1)
          b(n)   =  b(n)   - s(n-1)*c(n-1)/b(n-1)
          d(n)   =  d(n)   - s(n-1)*d(n-1)/b(n-1)
      endif

c...Solve the upper-triangular system using
c...back-substitution. 
      x(n) = d(n)/b(n)
      x(n-1) = (d(n-1)-c(n-1)*x(n))/b(n-1)
      do 20, i=n-2,1,-1
        x(i) = (d(i)-c(i)*x(i+1)-r(i)*x(n))/b(i)
 20   continue

      return
      end
