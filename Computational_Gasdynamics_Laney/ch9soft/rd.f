      program rd
c...Average-polynomial interpolation ENO reconstruction
c...via the primitive function.  Polynomial order controlled
c...by parameter m (e.g. m=3 corresponds to quadratic)

c...Boundaries periodic!

      parameter (nmax=200,n1max=20000,m=1+2,delta=1.D-10)
      double precision u(-m+1:nmax+m),x(1:nmax),xhalf(-m+1:nmax+m+1)
      double precision dd(m,-m+1:nmax+m),u1(n1max),x1(n1max)
      double precision errmax, errsum, sum, product
      double precision c(m,nmax),cp(m),cm(m)
      integer left

      open(unit=9,file='rd.out')

      open(unit=8,file='cell.dat',status='old')
      read(8,*) n
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
c...Read cell edges.
      do 10, i=1,n+1
        read(8,*,err=1000,end=1000) xhalf(i)
        if(i.gt.1.and.xhalf(i).lt.xhalf(i-1)-delta) then
          write(9,*) '****Samples out of order****'
          close(unit=8)
          close(unit=9)
          stop
        endif
 10   continue
c...Read cell integral averages
      do 11, i=1,n
        read(8,*,err=1000,end=1000) u(i)
 11   continue

c...Read evaluation points.
      read(8,*) n1
      if(n1.gt.n1max) then
        write(9,*) '****Too many evaluations****'
        close(unit=8)
        close(unit=9)
        stop
      endif
      if(n1.lt.n) then
        write(9,*) '****Too few evaluations****'
        close(unit=8)
        close(unit=9)
        stop
      endif
      iflag = 0
      do 12, i=1,n1
        read(8,*,err=1000,end=1000) x1(i), u1(i)
        if(iflag.eq.0.and.i.gt.1.and.x1(i).lt.x1(i-1)-delta) then
          write(*,*) '****Evaluation points out of order****'
          write(*,*) '****L1 error calculation innaccurate****'
          iflag = 1
        endif
        if(x1(i).gt.xhalf(n+1)+delta.or.x1(i).lt.xhalf(1)-delta) then
          write(9,*) '****Evaluation point outside the region ',
     *               'defined by cell edges****'
          write(9,*) i,':  x1=', x1(i)
          close(unit=8)
          close(unit=9)
          stop
        endif
 12   continue

      close(unit=8)

      if(abs(u1(n1)-u1(1)).gt.delta) then
        write(9,*) '****Evaluation points not periodic****'
        close(unit=9)
        stop
      endif

c...Enforce periodicity.
      do 15, i=1,m
        u(n+i) = u(i)
        xhalf(n+i+1) = xhalf(n+i) + (xhalf(i+1)-xhalf(i))
 15   continue

      do 16, i=0,m-1
        u(-i) = u(n-i)
        xhalf(-i) = xhalf(-i+1) - (xhalf(n-i+1)-xhalf(n-i))
 16   continue

c...Find cell centers.
      do 20, i=1,n
        x(i) = .5D0*(xhalf(i+1)+xhalf(i))
 20   continue

c...Compute divided differences
      do 30, i=-m+1,n+m
        dd(1,i) = u(i)
 30   continue
      do 40, j=2,m
        do 40, i=-m+1,n+m-j+1
          dd(j,i) = (dd(j-1,i+1)-dd(j-1,i))/(xhalf(i+j)-xhalf(i))
 40   continue


      do 55, j=1,n

c...Choose '+' interpolation for each cell j based on size of divided
c...differences.  To save space, specify interpolation by its
c...left-hand endpoint.
        left = j
        do 50, i=3,m
          if(abs(dd(i,left)).gt.abs(dd(i,left-1))) then
            left = left-1
          endif
 50     continue

c...Convert to Taylor series form
        call taylor(x(j),xhalf(left),dd(1,left),cp)

c...Choose '-' interpolation for each cell j based on size of divided
c...differences.  To save space, specify interpolation by its
c...left-hand endpoint.
        left = j-1
        do 51, i=3,m
          if(abs(dd(i,left)).gt.abs(dd(i,left-1))) then
            left = left-1
          endif
 51     continue

c...Convert to Taylor series form
        call taylor(x(j),xhalf(left),dd(1,left),cm)

c...Minmod average the Taylor series coefficients.
        do 52, i=1,m
          if(cm(i)*cp(i).lt.0.D0) then
            c(i,j) = 0.D0
          elseif(abs(cm(i)).lt.abs(cp(i))) then
            c(i,j) = cm(i)
          else
            c(i,j) = cp(i)
          endif
 52     continue

 55   continue


c...Evaluate reconstruction.
      errmax = 0.
      do 80, j=1,n1
        iflag = 0
c...Find which cell the evaluation point belongs in.
        do 60, i=1,n
          if(xhalf(i)-delta.lt.x1(j).and.
     *       xhalf(i+1)+delta.gt.x1(j)) then
            iflag = iflag+1
            icell = i
          endif
 60     continue
        if(iflag.lt.1) then
          write(*,*) '***Evaluation point not found in any cell:'
          write(*,*) '    x =', x1(j)
          write(*,*) 'Possible cure: increase delta'
          write(*,*) '------------------------------------------------'
        endif
        if(iflag.gt.2) then
          write(*,*) '***Evaluation point determined to lie',
     *     ' in more than one cell'
          write(*,*)  '   iflag =',iflag,'    x =', x1(j)
          write(*,*) 'Possible cure: reduce delta'
          write(*,*) '------------------------------------------------'
        endif
c...Evaluate the reconstruction on cell "icell."
        sum = 0.D0
        product = 1.D0
        do 70, i=1,m
          sum = sum + dble(i)*c(i,icell)*product
          product = product*(x1(j)-x(icell))
 70     continue       
        write(9,1100) x1(j), sum
        errmax = max(errmax,abs(u1(j)-sum))
        if(j.eq.1) then
          errsum = .5D0*abs(u1(j)-sum)*(x1(n1)-x1(n1-1)+
     .                                  x1(2 )-x1(1))
        endif
        if(j.gt.1.and.j.lt.n1) then
          errsum = errsum + .5D0*abs(u1(j)-sum)*(x1(j+1)-x1(j-1))
        endif
 80   continue

      write(*,*) 'Maximum error =',errmax
      write(*,*) 'Average (L1) error =', errsum/(xhalf(n+1)-xhalf(1))

      close(unit=9)

      stop

 1000 write(9,*) '****Error reading file interp.dat****'
      close(unit=8)
      close(unit=9)
      stop

 1100 format(f18.12,5x,f18.12,5x,f18.12)

      end


      subroutine taylor(xbar,xhalf,dd,c)

      parameter(m=1+2)

c...Find the interpolation polynomial in a Taylor series about xbar.

      double precision xhalf(m),c(m),d(0:m,0:m),dd(m),xbar

      do 10, i=0,m
        d(0,i) = 1.D0
 10   continue
      do 20, i=1,m
        d(i,0) = (xbar-xhalf(i))*d(i-1,0)
 20   continue
      do 30, i=1,m
        do 30, j=1,m-i
          d(i,j) = d(i,j-1) + (xbar-xhalf(i+j))*d(i-1,j)
 30   continue

      do 40, j=1,m
        c(j) = 0.
        do 40, i=0,m-j
          c(j) = c(j)+d(i,j)*dd(i+j)
 40   continue

      return
      end
