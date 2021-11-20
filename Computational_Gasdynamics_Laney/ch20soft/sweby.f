      program sweby

c...For and scalar conservation law, performs the method described
c...in Sweby, High Resolution Schemes Using Flux-Limiters for Hyperbolic
c...Conservation Laws, SIAM J. Numer. Anal., 21, 995 (1984).

      real*8 delta,dhartp
      parameter (nmax=1000,delta=.00000001,dhartp=0.4)
      real*8 delta_t, delta_x
      real*8 lambda,u(-2:nmax+3),h(0:nmax),u0(1:nmax+1)
      real*8 x,f,df, ap(-2:nmax+2),am(-2:nmax+2), rp(-2:nmax+2)
      real*8 rm(-2:nmax+2),epsilon(-2:nmax+2),aavg,t1,t2

c     f(x) = .5*x*x
c     df(x) = x

      f(x) = x
      df(x) = 1.

      write(*,*) 'which flux limiter?'
      write(*,*) '1 = van leer'
      write(*,*) '2 = superbee'
      write(*,*) '3 = minmod'
      write(*,*) '4 = van albada'
      write(*,*) '5 = symmetric minmod'
      write(*,*) '6 = asymmetric minmod'
      read(*,*) ifl

      open(unit=9,file='sweby.out')

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
          aavg = (f(u(i+1))-f(u(i)))/(u(i+1)-u(i))
        else
          rp(i) = sign(1.,(u(i)-u(i-1))*(u(i+1)-u(i)))/delta
          rm(i) = sign(1.,(u(i+2)-u(i+1))*(u(i+1)-u(i)))/delta
          aavg = df(u(i))
        endif

c...Choose Roe's first-order upwind method for use at sonic points.

c       epsilon(i) = abs(aavg)

c...Choose Harten's first-order upwind method for use at sonic points.
        if(abs(aavg).gt.dhartp) then
          epsilon(i) = abs(aavg)
        else
          epsilon(i) = .5*(aavg*aavg+dhartp*dhartp)/dhartp
        endif

        ap(i)=.5*(aavg+epsilon(i))
        am(i)=.5*(aavg-epsilon(i))

 90   continue

      do 95, i=0,n+1

        if(abs(ap(i)*(1.-lambda*ap(i))).gt.delta) then
          rp(i) = rp(i)*ap(i-1)*(1.-lambda*ap(i-1))
          rp(i) = rp(i)/(ap(i)*(1.-lambda*ap(i)))
        else
          rp(i) = sign(1.,(rp(i)*ap(i-1)*(1.-lambda*ap(i-1)))
     *    *(ap(i)*(1.-lambda*ap(i))))/delta
        endif
        if(abs(am(i)*(1.+am(i))).gt.delta) then
          rm(i) = rm(i)*am(i+1)*(1.+lambda*am(i+1))
          rm(i) = rm(i)/(am(i)*(1.+lambda*am(i)))
        else
          rm(i) = sign(1.,(rm(i)*am(i+1)*(1.+lambda*am(i+1)))
     *    *(am(i)*(1.+lambda*am(i))))/delta
        endif

        if(ifl.eq.1) then
c...van Leer's limiter
        rp(i) = 2.*rp(i)/(1.+rp(i))
        rp(i) = max(0.,rp(i))
        rm(i) = 2.*rm(i)/(1.+rm(i))
        rm(i) = max(0.,rm(i))
        elseif(ifl.eq.2) then
c...Superbee limiter
        t1 = min(2.*rp(i),1.)
        t2 = min(rp(i),2.)
        rp(i) = max(0.,t1,t2)
        t1 = min(2.*rm(i),1.)
        t2 = min(rm(i),2.)
        rm(i) = max(0.,t1,t2)
        elseif(ifl.eq.3) then
c...Minmod limiter
        rp(i) = max(0.,min(rp(i),1.))
        rm(i) = max(0.,min(rm(i),1.))
        elseif(ifl.eq.4) then
c...van Albada limiter
        rp(i) = rp(i)*(1.+rp(i))/(1.+rp(i)*rp(i))
        rm(i) = rm(i)*(1.+rm(i))/(1.+rm(i)*rm(i))
        elseif(ifl.eq.5) then
c...Symmetric Minmod
        rp(i) = min(1.,abs(rp(i)))
        rm(i) = min(1.,abs(rm(i)))
        elseif(ifl.eq.6) then
        if(rp(i).lt.-1.) then
          rp(i) = -1.
        elseif(rp(i).gt.1.) then
          rp(i) = 1.
        endif
        if(rm(i).lt.-1.) then
          rm(i) = -1.
        elseif(rm(i).gt.1.) then
          rm(i) = 1.
        endif
        endif

 95   continue

      do 100, i=0,n
        h(i) = .5*(f(u(i+1))+f(u(i)))
     *         -.5*epsilon(i)*(u(i+1)-u(i))
     *         +.5*ap(i)*(1.-lambda*ap(i))*rp(i)*(u(i+1)-u(i))
     *         -.5*am(i)*(1.+lambda*am(i))*rm(i)*(u(i+1)-u(i))
 100  continue

      do 120, i=1,n
        u(i) = u(i)-lambda*(h(i)-h(i-1))
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
      open(unit=10,file='sweby.plt')
      do 900, i=1,n+1
        write(10,1150) -1 + real(2*i-2)/real(n), u(i)
 900  continue
      close(unit=10)

      stop

 1000 write(9,*) '****Error reading data point number ', i,'****'
      close(unit=8)
      close(unit=9)
      stop

 1050 format(4x,'N',11x,'INITIAL',11x,'SWEBY',10x, 'DIFFERENCE')
 1100 format(i5,5x,f13.8,5x,f13.8,5x,f13.8)
 1150 format(f14.9,5x,f14.9)

      end
