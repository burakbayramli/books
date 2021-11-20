      program enosho
      
c...Performs finite-difference ENO as in Shu and Osher.

c...Boundaries periodic!

c...Allows non-constant grid-spacing.

      double precision delta
      integer nmax,m,itmax
C...If you change the order-of-accuracy m here, you must also change it in "spatial"
C...and "taylor."	 If you make m>4, you must input Runge-Kutta coefficients below.
      parameter (nmax=800,m=2+1,delta=1.D-8,itmax=100000)
      double precision u(-m+1:nmax+m), u0(nmax+1), uu(-m+1:nmax+m)
      double precision xhalf(-m+1:nmax+m+1)
      double precision c(nmax+1), rk(4,4), ll(nmax,m)
      double precision sum, rmax, f, df, r, x(nmax+1)
      double precision cfl,lambda(nmax+1),t,delta_t,tfinal

c...Define flux function and its derivative.	If you change the flux function here,
c...you must also change it in subroutine "spatial."

c     f(r) = .5D0*r*r
c     df(r) = r
      f(r) = r
      df(r) = 1.D0
      
      open(unit=9,file='enosho.out')
      open(unit=8,file='cell.dat',status='old')

      read(8,*) n, cfl, tfinal
      read(8,*) ice
      if(n.gt.nmax) then
        write(9,*) '****Too many cells****'
        close(unit=8)
        close(unit=9)
        stop
      endif
      if(n.lt.2) then
        write(9,*) '****Too few cells****'
        close(unit=8)
        close(unit=9)
        stop
      endif
      if(cfl.lt.0.01) then
        write(9,*) '****CFL number small or negative****'
        close(unit=8)
        close(unit=9)
        stop
      endif
      if(tfinal.lt.0.01) then
        write(9,*) '****Final time small or negative****'
        close(unit=8)
        close(unit=9)
        stop
      endif
 
      if(ice.eq.1) then

c...Read cell edges.
        do 5, i=1,n+1
          read(8,*,err=1000,end=1000) xhalf(i)
          if(i.gt.1.and.xhalf(i).lt.xhalf(i-1)-delta) then
            write(9,*) '****Samples out of order****'
            close(unit=8)
            close(unit=9)
            stop
          endif
 5        continue

c...Define cell centers
          do 6, i=1,n
            x(i) = .5*(xhalf(i+1)+xhalf(i))
 6        continue

c...Enforce periodicity in x.
          do 7, i=1,m
            xhalf(n+i+1) = xhalf(n+i) + (xhalf(i+1)-xhalf(i))
 7        continue
          do 8, i=0,m-1
            xhalf(-i) = xhalf(-i+1) - (xhalf(n-i+1)-xhalf(n-i))
 8        continue

        elseif(ice.eq.-1) then

c...Read cell centers.
          do 9, i=1,n+1
            read(8,*,err=1000,end=1000) x(i)
            if(i.gt.1.and.x(i).lt.x(i-1)-delta) then
              write(9,*) '****Samples out of order****'
              close(unit=8)
              close(unit=9)
              stop
            endif
 9        continue

c...Define cell edges.
          xhalf(2) = .5D0*(x(2)+x(1))
          do 10, i=2,n
            xhalf(i+1) = 2.D0*x(i)  - xhalf(i)
            if(xhalf(i+1).gt.x(i+1).or.xhalf(i+1).lt.x(i)) then
              write(9,*)  '****Unable to find cell edges (1)****'
              write(9,*)  'xhalf(i+1/2) =', xhalf(i+1)
              write(9,*)  'x(i)=',x(i), '      x(i+1)=',x(i+1)
              close(unit=8)
              close(unit=9)
              stop
            endif
 10       continue
          
c...Enforce periodicity in x.
          do 11, i=-1,m-1
            xhalf(-i) = xhalf(n-i) - (x(n+1)-x(1))
 11       continue
          do 12, i=0,m
            xhalf(n+i+1) = xhalf(i+1) + (x(n+1)-x(1))
 12       continue

          if(abs(xhalf(n+1)-xhalf(1)-(x(n+1)-x(1))).gt.delta) then
            write(9,*)  '****Unable to find cell edges (2)****'
            close(unit=8)
            close(unit=9)
            stop
          endif

        else

          write(9,*) 'Must have -1 or 1 on second line of cell.dat'
          write(9,*) ' 1 = cell-edges specified'
          write(9,*) '-1 = cell-centers specified'

        endif

c...Read and save initial cell-centered-samples.
      do 15, i=1,n
        read(8,*,err=1000,end=1000) u(i)
        u0(i) = u(i)
 15   continue
      if(ice.eq.-1) u0(n+1) = u(1)

c...Enforce periodicity in u.
        do 40, i=1,m
          u(n+i) = u(i)
 40     continue
        do 45, i=0,m-1
          u(-i) = u(n-i)
 45     continue

c...Define Runge-Kutta Coefficients
      if(m.eq.1) then
c...Forward-Euler.
        rk(1,1) = 1.D0
        if(cfl.gt.1.D0) then
          write(9,*) 'Warning: CFL# too large'
        endif
      elseif(m.eq.2) then
c...Improved Euler method.
        rk(1,1) = 1.D0
        rk(1,2) = .5D0
        rk(2,2) = .5D0
        if(cfl.gt.1.D0) then
          write(9,*) 'Warning: CFL# too large'
        endif
      elseif(m.eq.3) then
c...Heun's 3rd-order formula.
c       rk(1,1) = 1.D0/3.D0
c       rk(1,2) = 0.D0
c       rk(2,2) = 2.D0/3.D0
c       rk(1,3) = .25D0
c       rk(2,3) = 0.D0
c       rk(3,3) = .75D0
c...Kutta's 3rd-order formula.
c       rk(1,1) = .5D0
c       rk(1,2) = -1.D0
c       rk(2,2) =  2.D0
c       rk(1,3) =  1.D0/6.D0
c       rk(2,3) =  2.D0/3.D0
c       rk(3,3) =  1.D0/6.D0
c...Shu and Osher's 3rd-order R-K method.
        rk(1,1) = 1.D0
        rk(1,2) = .25D0
        rk(2,2) = .25D0
        rk(1,3) = 1.D0/6.D0
        rk(2,3) = 1.D0/6.D0
        rk(3,3) = 2.D0/3.D0
        if(cfl.gt.1.D0) then
          write(9,*) 'Warning: CFL# too large (?)'
        endif
      elseif(m.eq.4.) then
        rk(1,1) = .5D0
        rk(1,2) = 0.D0
        rk(2,2) = .5D0
        rk(1,3) = 0.D0
        rk(2,3) = 0.D0
        rk(3,3) = 1.D0
        rk(1,4) = 1.D0/6.D0
        rk(2,4) = 1.D0/3.D0
        rk(3,4) = 1.D0/3.D0
        rk(4,4) = 1.D0/6.D0
        if(cfl.gt.2.D0/3.D0) then
          write(9,*) 'Warning: CFL# probably too large'
        endif
      else       
        write(9,*) '****Runge-Kutta not implemented****'
        close(unit=8)
        close(unit=9)
        stop
      endif

      rmax = 1.e10
      do 35, i=1,n
        rmax = min ( rmax, xhalf(i+1)-xhalf(i) )
 35   continue
      delta_t= cfl*rmax

      do 50, i=1,n
         lambda(i) = delta_t/(xhalf(i+1)-xhalf(i))
 50   continue
      lambda(n+1) = lambda(1)
      

c...TIME LOOP.
      t = 0.D0
      do 500, it=1,itmax

c...Find delta_t.
c       rmax = 0.D0
c       do 35, i=1,n
c         rmax = max(rmax, abs(df(u(i))) / (xhalf(i+1)-xhalf(i))  )
c35     continue
c       delta_t = cfl/rmax
c       if(t.lt.tfinal.and.t+delta_t.gt.tfinal) then
c         delta_t = tfinal-t
c       endif
        t = t+delta_t

        if(t.gt.tfinal+delta.or.it.eq.itmax) then
c...PRINT OUT FINAL RESULTS AND STOP
          write(9,*) 'Final time requested =', tfinal
          write(9,*) 'Actual final time =',t-delta_t
          write(9,*) '# time steps =', it-1
          write(9,*) 'FOR CONSTANT DELTA_X:'
          write(9,*) 'delta_x =', xhalf(2)-xhalf(1)
          write(9,*) 'delta_t =', delta_t
          write(9,*) 'FOR LINEAR FLUX FUNCTION AND EVEN NUMBER OF ',
     *              'TIMES AROUND THE PERIODIC DOMAIN:'
          write(9,*) 'Maximum error =', rmax
          if(ice.eq.1) then
            write(9,*) 'Average (L1) error =',sum/(xhalf(n+1)-xhalf(1))
          else
            write(9,*) 'Average (L1) error =',sum/(x(n+1)-x(1))
          endif

          sum = 0.D0
          rmax = 0.D0
          do 600, i=1,n
            sum = sum + abs(u(i)-u0(i))*(xhalf(i+1)-xhalf(i))
            rmax = max(rmax,abs(u(i)-u0(i)))
            write(9,1100) x(i),u(i),u0(i)
 600      continue

          if(ice.eq.-1) write(9,1100) x(n+1),u(n+1),u0(n+1)

C...write simple file for plotting
          open(unit=10,file='enosho.plt')
          do 900, i=1,n+1
           write(10,1150) x(i),u(i)
 900      continue
          close(unit=10)

          stop

        endif
        
c...Find lambda.
c       do 50, i=1,n
c         lambda(i) = delta_t/(xhalf(i+1)-xhalf(i))
c50     continue
c       lambda(n+1) = lambda(1)

c...RUNGE-KUTTA METHOD.

c...Find u[1]:
        call spatial(1,n,xhalf,u,lambda,c)
        do 60, j=1,n
          ll(j,1) = lambda(j)*(-c(j+1)+c(j))
          uu(j) = u(j) + rk(1,1)*ll(j,1)
 60     continue 
c...Enforce periodicity.
        do 70, j=1,m
          uu(n+j) = uu(j)
 70     continue
        do 80, j=0,m-1
          uu(-j) = uu(n-j)
 80     continue

c...Find u[i], i=2,m:
        do 120, i=2,m
          call spatial(1,n,xhalf,uu,lambda,c)
          do 90, j=1,n
            ll(j,i) = lambda(j)*(-c(j+1)+c(j))
            uu(j) = u(j)
            do 90, k=1,i
              uu(j) = uu(j) + rk(k,i)*ll(j,k)
 90       continue
c...Enforce periodicity.
          do 100, j=1,m
            uu(n+j) = uu(j)
 100       continue
          do 110, j=0,m-1
            uu(-j) = uu(n-j)
 110      continue
 120    continue

c...Update the solution.
        do 130, j=-m+1,n+m
          u(j) = uu(j)
 130     continue
             
 500  continue

 1000 write(9,*) '****Error reading file cell.dat****'
      close(unit=8)
      close(unit=9)

      stop

 1100 format(f18.12,5x,f18.12,5x,f18.12)
 1150 format(f14.9,5x,f14.9)

      end

      subroutine spatial(ncall,n,xhalf,u,lambda,c)

      parameter(m=2+1,nmax=800)
      double precision u(-m+1:nmax+m), delta
      double precision xhalf(-m+1:nmax+m+1)
      double precision ddf(m,-m+1:nmax+m),ddu(m,-m+1:nmax+m)
      double precision ddp(m,-m+1:nmax+m),ddm(m,-m+1:nmax+m)
      double precision c(nmax+1),cp,cm,lambda(nmax+1)
      double precision f,df,r,alpha,tmp1,tmp2
      integer left(nmax),leftp(nmax),leftm(nmax),iflag(nmax)
      parameter(delta=8.d-8)

      save left,leftp,leftm,iflag

c...Define flux function
c     f(r) = .5D0*r*r
c     df(r) = r
      f(r) = r
      df(r) = 1.D0

c...Compute divided differences of f and u.
      do 10, i=-m+1,n+m
        ddf(1,i) = f(u(i))
        ddu(1,i) = u(i)
 10   continue
      do 20, j=2,m
        do 20, i=-m+1,n+m-j+1
          ddf(j,i) = (ddf(j-1,i+1)-ddf(j-1,i))/(xhalf(i+j)-xhalf(i))
          ddu(j,i) = (ddu(j-1,i+1)-ddu(j-1,i))/(xhalf(i+j)-xhalf(i))
 20   continue

      do 100, j=1,n

c       if(ncall.eq.1) then
c         if(df(u(j+1))*df(u(j)).lt.0.D0) then
c         if(df(u(j)).lt.0.D0.and.df(u(j+1)).gt.0.D0) then
            iflag(j) = -1
c         else
c           iflag(j) = 1
c         endif
c       endif

c       iflag(j) = 2

        if(iflag(j).eq.-1) then

c...LINEAR SPLITTING.

c...Choose linear splitting parameter.

c...Lax-Friedrichs...
c         alpha = 1.D0/min(lambda(j),lambda(j+1))
c         alpha = 1.D0
c...Near maximum of |f'(u)|, sort of like S&O's choice...
          tmp1 = abs(df(u(j+1))) 
          tmp2 = abs(df(u(j))) 
          alpha = max(tmp1,tmp2)

c...Calculate divided differences of split fluxes.
          do 30, k=1,m
c           do 30, i=-m+1,n+m-k+1
            do 30, i=j-m,j+m-k+1
              ddp(k,i) = .5D0*(ddf(k,i) + alpha*ddu(k,i))
              ddm(k,i) = .5D0*(ddf(k,i) - alpha*ddu(k,i))
 30       continue

c...Choose '+' interpolation for each cell j based on size of divided
c...differences.  To save space, specify interpolation by its
c...left-hand endpoint.
          if(ncall.eq.1) then
          leftp(j) = j
          do 40, i=2,m
            if(abs(ddp(i,leftp(j))).gt.abs(ddp(i,leftp(j)-1))) then
              leftp(j) = leftp(j)-1
            endif
 40       continue
          endif

c...Convert to Taylor series form (find first derivative)
          call taylor(xhalf(j+1),xhalf(leftp(j)),ddp(1,leftp(j)),cp)

c...Choose '-' interpolation for each cell j based on size of divided
c...differences.  To save space, specify interpolation by its
c...left-hand endpoint.
          if(ncall.eq.1) then
          leftm(j) = j+1
          do 50, i=2,m
            if(abs(ddm(i,leftm(j))).gt.abs(ddm(i,leftm(j)-1))) then
              leftm(j) = leftm(j)-1
            endif
 50       continue
          endif

c...Convert to Taylor series form (find first derivative)
          call taylor(xhalf(j+1),xhalf(leftm(j)),ddm(1,leftm(j)),cm)

c...Add the '+' and '-' coefficients.
          c(j+1) = cp+cm

        elseif(iflag(j).eq.1) then

C...FULLY UPWIND SPLITTING
          if(ncall.eq.1) then
          if(abs(u(j+1)-u(j)).lt.delta) then
            if(df(u(j)).ge.0.D0) then
              left(j) = j
            else
              left(j) = j+1
            endif
          elseif((f(u(j+1))-f(u(j)))*(u(j+1)-u(j)).gt.0.D0) then
            left(j) = j
          else
            left(j) = j+1
          endif   
          endif

          do 60, i=2,m
            if(abs(ddf(i,left(j))).gt.abs(ddf(i,left(j)-1))) then
              left(j) = left(j)-1
            endif
 60       continue

          call taylor(xhalf(j+1),xhalf(left(j)),ddf(1,left(j)),c(j+1))

        elseif(iflag(j).eq.2) then

C...NO SPLITTING (possibly unstable!)
          if(ncall.eq.1) then
c         left(j) = j+1
          left(j) = j

          do 70, i=2,m
            if(abs(ddf(i,left(j))).gt.abs(ddf(i,left(j)-1))) then
              left(j) = left(j)-1
            endif
 70       continue
          endif

          call taylor(xhalf(j+1),xhalf(left(j)),ddf(1,left(j)),c(j+1))

        endif

 100  continue

      c(1) = c(n+1)

      return
      end

      subroutine taylor(xbar,xhalf,dd,c)

      parameter(m=2+1)

c...Find the interpolation polynomial in a Taylor series about xbar.
c...To save time, only compute the FIRST coefficient.

      double precision xhalf(m),c,d(0:m),dd(m),xbar

      d(0) = 1.D0
      do 20, i=1,m
        d(i) = (xbar-xhalf(i))*d(i-1)
 20   continue
      do 30, i=1,m-1
        d(i) = d(i) + (xbar-xhalf(i+1))*d(i-1)
 30   continue

      c = 0.
      do 40, i=0,m-1
        c = c+d(i)*dd(i+1)
 40   continue

      return
      end
