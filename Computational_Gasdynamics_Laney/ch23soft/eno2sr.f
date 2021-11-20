      program eno2sr
c...Performs 2nd-order Essentially Non-Oscillatory (ENO) method
c...described in Harten, Engquist, Chakravarthy, and Osher,
c...J. Comput. Phys., vol. 71, 231-303 (1987)
c...Subcell resolution performed at discontinuities as described
c...in Harten, J. Comput. Phys., vol. 83, 148-184 (1989)

      parameter (nmax=100,delta=.000001)
      real lambda,u(-2:nmax+2),h(0:nmax),u0(1:nmax+1),S(-1:nmax+1)
      real delta_u(-2:nmax+1),delta2_u(-1:nmax+2),a(-1:nmax+1)
      real minmod,mm,g1(0:nmax),g2(-1:nmax),sigma(-1:nmax+1)
      real C(-1:nmax+1)

c...If f(x) is redefined here, it must also be redefined in function
c..."riemann."  Also, all sonic points (max and min of f) must be
c...specified in "riemann."
      f(x) =   x
      df(x) =  1.

      open(unit=9,file='eno2sr.out')

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
        delta2_u(i) = delta_u(i)-delta_u(i-1)
 60   continue
      delta2_u(n+2) = delta2_u(2)

c...Define wave speeds at cell centers 
      do 70, i=-1,n+1
        a(i) = lambda*df(u(i))
 70   continue

      iflag = 1

      do 80, i=0,n+1

      if(iflag.eq.0) then
c...Reconstruction via Deconvolution (RD)
        t1 = mm(delta2_u(i),delta2_u(i+1))
        t2 = mm(delta2_u(i-1),delta2_u(i))
        S(i) = minmod(delta_u(i)-.5*t1,delta_u(i-1)+.5*t2)
        C(i) = 0.
      else
c...Reconstruction via Primitive Function.
        if(abs(delta_u(i)).lt.abs(delta_u(i-1))) then
          i2 = i
        else
          i2 = i-1
        endif

        if(abs(delta2_u(i2+1)).lt.abs(delta2_u(i2))) then
          i3 = i2+1
        else
          i3 = i2
        endif

        S(i) = delta_u(i2) + delta2_u(i3)*(real(i-i2)-.5)

c       C(i) = delta2_u(i3)
        C(i) = 0.
      endif
      S(-1) = S(n-1)
      C(-1) = C(n-1)
        
 80   continue

      do 90, i=0,n
        h(i) = lambda*riemann( u(i)+.5*(1.-a(i))*S(i),
     .                       u(i+1)-.5*(1.+a(i+1))*S(i+1))
 90   continue    


c...Apply subcell resolution

c...Define variable for discontinuity test
      do 100, i=-1,n+1
        sigma(i) = abs(S(i))
 100  continue

      do 110, i=0,n
c...Possible discontinuity in cell i if sigma(i) is a local maximum.
        if(sigma(i).gt.sigma(i+1).and.sigma(i).gt.sigma(i-1)) then
c...Determine F(theta) at edges of cell i.
          fl =  delta_u(i)  -S(i+1)
          fr = -delta_u(i-1)+S(i-1)
c...Discontinuity in cell i considered confirmed if F switches sign.
          if(fl*fr.lt.0.) then
            if(a(i).gt.0.) then
              g2(i-1) = 0.
              fxat = (1.-a(i))*(u(i-1)+.5*(2.-a(i))*S(i-1))
     .              + a(i)*(u(i+1)-.5*(1.+a(i))*S(i+1)) -u(i)
              if(fxat*fl.lt.0.) then
                g1(i) = a(i+1)*(u(i+1)-.5*S(i+1)*(1.+a(i+1)))
     .                      -a(i)*(u(i)+.5*S(i)*(1.-a(i)))
     .               +a(i+1)*(a(i+1)+1.)*(2.*a(i+1)+1.)*C(i+1)/12.
              else
                g1(i) = a(i-1)*(u(i-1)+.5*S(i-1)*(3.-a(i-1)))
     .                      -a(i)*(u(i)+.5*S(i)*(1.-a(i)))
     .                      + u(i)-u(i-1)-S(i-1)-.5*C(i-1)   
     .           +a(i-1)*(13.-9.*a(i-1)+2.*a(i-1)*a(i-1))*C(i-1)/12.
              endif
            else
              g1(i) = 0.
              fxat = -a(i)*(u(i-1)+.5*(1.-a(i))*S(i-1))
     .             +(1.+a(i))*(u(i+1)-.5*(2.+a(i))*S(i+1)) -u(i)
              if(fxat*fr.lt.0.) then
                g2(i-1) = a(i-1)*(u(i-1)+.5*S(i-1)*(1.-a(i-1)))
     .                  -a(i)*(u(i)-.5*S(i)*(1.+a(i)))
     .               +a(i-1)*(a(i-1)-1.)*(2.*a(i-1)-1.)*C(i-1)/12.
              else
                g2(i-1) = a(i+1)*(u(i+1)-.5*S(i+1)*(3.+a(i+1)))
     .                  -a(i)*(u(i)-.5*S(i)*(1.+a(i)))
     .                     + u(i+1)-u(i)-S(i+1) + .5*C(i+1)
     .           +a(i+1)*(13.+9.*a(i+1)+2.*a(i+1)*a(i+1))*C(i+1)/12.
              endif
            endif
          else
            if(a(i).gt.0.) then
              g1(i) = a(i)*(a(i)-1.)*(2.*a(i)-1.)*C(i)/12.
              g2(i-1) = 0.
            else
              g1(i) = 0.
              g2(i-1) = a(i)*(a(i)+1.)*(2.*a(i)+1.)*C(i)/12.
            endif
          endif
        else
          if(a(i).gt.0.) then
            g1(i) = a(i)*(a(i)-1.)*(2.*a(i)-1.)*C(i)/12.
            g2(i-1) = 0.
          else
            g1(i) = 0.
            g2(i-1) = a(i)*(a(i)+1.)*(2.*a(i)+1.)*C(i)/12.
          endif
        endif
 110  continue
      g2(n) = g2(0)

      do 120, i=1,n
        u(i) = u(i)-h(i)-g1(i)-g2(i)+h(i-1)+g1(i-1)+g2(i-1)
 120  continue

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
      open(unit=10,file='eno2sr.plt')
c     write(10,*) '2nd order ENO-SR'
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

 1050 format(4x,'N',11x,'INITIAL',11x,'ENO-2SR',8x, 'DIFFERENCE')
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

      f(x) =  x

c...Enter the location of sonic points (max and min of f(x)).
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
      
