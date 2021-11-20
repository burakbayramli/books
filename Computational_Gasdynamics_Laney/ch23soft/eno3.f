      program eno3
c...Performs 3rd-order Essentially Non-Oscillatory (ENO) method
c...described in Harten, Engquist, Chakravarthy, and Osher,
c...J. Comput. Phys., vol. 71, 231-303 (1987)
      parameter (nmax=160,delta=.000001,irtype=1)
      real lambda,u(-2:nmax+2),h(0:nmax),u0(1:nmax+1),S(0:nmax+1)
      real delta_u(-2:nmax+2),delta2_u(-2:nmax+1),a(-1:nmax+1)
      real minmod,mm,C(0:nmax+1),delta3_u(-2:nmax)

C...MUST USE LINEAR FLUX FUNCTION!!! 
      f(x) =  x
      df(x) =  1.

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
      delta_u(n+2) = delta_u(2)

      do 51, i=-2,n+1
        delta2_u(i) = delta_u(i+1)-delta_u(i)
 51   continue

      do 52, i=-2,n
        delta3_u(i) = delta2_u(i+1)-delta2_u(i)
 52   continue

      do 70, i=-1,n+1
        if(abs(delta_u(i)).gt.delta) then
          a(i) = lambda*(f(u(i+1))-f(u(i)))/delta_u(i)
        else
          a(i) = lambda*df(u(i))
        endif
 70   continue

      do 80, i=0,n

        if(irtype.eq.0) then
c...Reconstruction via Deconvolution (RD)

c...'+' reconstruction
        if(abs(delta2_u(i)).le.abs(delta2_u(i-1))) then
          i2p = i
          r1p = 1./3. 
          r2p = -1.
        else
          i2p = i-1
          r1p = -1./6. 
          r2p = 0.
        endif

        if(abs(delta3_u(i2p)).le.abs(delta3_u(i2p-1))) then
          i3p = i2p
        else
          i3p = i2p-1
        endif

c...'-' reconstruction
        if(abs(delta2_u(i-1)).le.abs(delta2_u(i-2))) then
          i2m = i-1
          r1m = -1./6. 
          r2m = 0.
        else
          i2m = i-2
          r1m = 1./3. 
          r2m = 1.
        endif

        if(abs(delta3_u(i2m)).le.abs(delta3_u(i2m-1))) then
          i3m = i2m
        else
          i3m = i2m-1
        endif

c...Average positive and negative reconstructions
        tp = delta_u(i)   - .5*delta2_u(i2p) + r1p*delta3_u(i3p)
        tm = delta_u(i-1) + .5*delta2_u(i2m) + r1m*delta3_u(i3m)

        S(i) = minmod(tp,tm)
c       S(i) = mm(tp,tm)

        tp = delta2_u(i2p) + r2p*delta3_u(i3p)
        tm = delta2_u(i2m) + r2m*delta3_u(i3m)

        C(i) = minmod(tp,tm)
c       C(i) = mm(tp,tm)

	  elseif(irtype.eq.1) then
c...Reconstruction via Primitive Function (RP).

        if(abs(delta_u(i)).le.abs(delta_u(i-1))) then
          i2 = i
          r1 = -.5 
        else
          i2 = i-1
          r1 =  .5
        endif

        if(abs(delta2_u(i2)).le.abs(delta2_u(i2-1))) then
          i3 = i2
        else
          i3 = i2-1
        endif

        if(i3.eq.i) then
          r2 = 1./3.
          r3 = -1.
        elseif(i3.eq.i-1) then
          r2 = -1./6.
          r3 = 0.
        elseif(i3.eq.i-2) then
          r2 = 1./3.
          r3 = 1.
        endif

        if(abs(delta3_u(i3)).le.abs(delta3_u(i3-1))) then
          i4 = i3
        else
          i4 = i3-1
        endif

        S(i) = delta_u(i2) + r1*delta2_u(i3) + r2*delta3_u(i4)

        C(i) = delta2_u(i3) + r3*delta3_u(i4)

        endif

 80   continue

c...Assume a=const.>0
      do 90, i=0,n
        t = u(i) + .5*(1.-a(i))*S(i) + (1.-3.*a(i)+2*a(i)*a(i))*C(i)/12.
        h(i) = a(i)*t
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
      open(unit=10,file='eno3.plt')
c     write(10,*) '3rd order ENO'
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

 1050 format(4x,'N',11x,'INITIAL',11x,'ENO-3',10x, 'DIFFERENCE')
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

      real function mm(x,y)

      if(abs(x).ge.abs(y)) then
        mm = y
      else
        mm = x
      endif
       
      return
      end

      real function riemann(x,y)
      parameter(nsonic=3)
      real sonic(1:nsonic)

      f(x) = x

c...Enter the location of sonic points (max and min of f(x)):
c...(It is ok to have additional false sonic points. In fact, it is 
c...convenient, so that the list includes all of the sonic points
c...for any likely choice of f)
      sonic(1)=.5
      sonic(2)= 0.
      sonic(3)=-.5

      if(x.le.y) then
        rm = 10.E20
        do 10, i=1,nsonic
          if(x.le.sonic(i).and.sonic(i).le.y) then
             rm = min(rm,f(sonic(i)))
          endif
 10     continue
        riemann = min(f(x),f(y),rm)
      else
        rm = -10.E20
        do 20, i=1,nsonic
          if(x.ge.sonic(i).and.sonic(i).ge.y) then
             rm = max(rm,f(sonic(i)))
          endif
 20     continue
        riemann = max(f(x),f(y),rm)
      endif

      return
      end
      
