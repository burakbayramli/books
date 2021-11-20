      program vl4
c...For the linear advection equation, performs the method described
c...in van Leer, Towards the Ultimate Conservative Difference Scheme. 
c...IV. A New Approach to Numerical Convection, J. Comput. Phys.,
c...23, 276-299 (1974).
      parameter (nmax=100,delta=.000001)
      real lambda,u(-2:nmax+2),h(0:nmax+1),u0(1:nmax+1),s(0:nmax)

      open(unit=9,file='vl4.out')

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

c...Compute limited slope using minmod of adjacent slopes.
      do 80, i=0,n
        sn = sign(1.,u(i+1)-u(i-1))
c       temp = min(2.*sn*(u(i)-u(i-1)),.5*sn*(u(i+1)-u(i-1)),
c    *             2.*sn*(u(i+1)-u(i)))
        temp = min(2.*sn*(u(i)-u(i-1))/lambda,.5*sn*(u(i+1)-u(i-1)),
     *             2.*sn*(u(i+1)-u(i))/(1.-lambda))
        s(i) = sn*max(0.,temp)
c       s(i) = .5*(u(i+1)-u(i-1))
 80   continue

      do 90, i=0,n
        s1 = lambda
        s2 = .5*lambda*(1.-lambda)
        h(i) = s1*(u(i)-u(i-1))+s2*(s(i)-s(i-1))
 90   continue

      do 100, i=1,n
        u(i) = u(i)-h(i)
 100  continue

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
      open(unit=10,file='vl4.plt')
c     write(10,*) 'MUSCL Based on Fromm''s Method (No CFL# In Limiter)'
c     write(10,*) 'MUSCL Based on Fromm''s Method'
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

 1050 format(4x,'N',11x,'INITIAL',12x,'VL2',11x, 'DIFFERENCE')
 1100 format(i5,5x,f13.8,5x,f13.8,5x,f13.8)
 1150 format(f14.9,5x,f14.9)

      end
