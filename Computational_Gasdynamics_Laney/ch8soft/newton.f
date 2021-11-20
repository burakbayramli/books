      program newton

c...Polynomial reconstruction in Newton form.

      parameter(nmax=300,n1max=20000)
      double precision a(nmax),dd(nmax,nmax-1),sum,rmax
      double precision x(nmax),y(nmax),yy,t1,x2(n1max),y2(n1max)

      open(unit=9,file='newton.out')
      open(unit=10,file='interp.dat',status='old')

c...Read points for interpolation
      read(10,*) n
      if(n.gt.nmax) then
        write(9,*) n
        write(9,*) 'Too many interpolation points---increase nmax'
        stop
      endif
      do 10, i=1,n
        read(10,*) x(i),y(i)
 10   continue
      read(10,*) n1
      if(n1.gt.n1max) then
        write(9,*) n
        write(9,*) 'Too many plotting points---increase n1max'
        stop
      endif
      do 11, i=1,n1
        read(10,*) x2(i),y2(i)
 11   continue
      

      close(unit=10)

c...Compute first divided differences about the first point.
c     do 20, i=2,n
c       dd(i,1) = (y(i)-y(1))/(x(i)-x(1))
c20   continue

c...Compute higher order divided differences about the first point.
c     do 30, j=2,n-1
c       do 30, i=j+1,n
c         dd(i,j) = (dd(i,j-1)-dd(i-1,j-1))/(x(i)-x(i-j+1))
c30   continue

c...EQUIVALENTLY...
c...Compute standard Newton first divided differences.
      do 20, i=2,n
        dd(i,1) = (y(i)-y(i-1))/(x(i)-x(i-1))
 20   continue

c...Compute standard Newton higher order divided differences.
      do 30, j=2,n-1
        do 30, i=j+1,n
          dd(i,j) = (dd(i,j-1)-dd(i-1,j-1))/(x(i)-x(i-j))
 30   continue

c...Form Newton coefficients. 
      do 70, j=1,n-1
        a(j) = dd(j+1,j)
 70   continue

      write(9,*) 'Newton coefficients'
      write(9,450) y(1)
      do 75, i=1,n-1
        write(9,450) a(i)
 75   continue
      write(9,*) '----------------------------------------------'
      write(9,*) 'Polynomial Evaluated at Interpolation Points'
 
      do 90, j=1,n
        t1 = x(j)-x(1)
        yy = y(1)
        do 80, i=1,n-1
          yy = yy+a(i)*t1
          t1 = t1*(x(j)-x(i+1))
 80     continue
        write(9,500) x(j),y(j),yy-y(j)
 90   continue

      open(unit=11,file='newton.plt')

      sum = 0.
      rmax = -1.
      do 110, j=1,n1
        t1 = x2(j)-x(1)
        yy = y(1)
        do 100, i=1,n-1
          yy = yy+a(i)*t1
          t1 = t1*(x2(j)-x(i+1))
 100    continue
        write(11,500) x2(j),yy
        if(j.ne.1) sum = sum + 
     .             (x2(j)-x2(j-1))*dabs(y2(j)-yy)*dabs(y2(j)-yy)
        rmax = dmax1(rmax,dabs(y2(j)-yy))
 110  continue
      write(9,*) '----------------------------------------------'
      write(9,*) 'L2 ERROR (assuming constant grid spacing) = ',
     .     sqrt ( sum)
      write(9,*) 'L-INFINITY ERROR = ',rmax

      close(unit=9)
      close(unit=11)

 450  format(D18.9)
 500  format(D18.9,3x,D18.9,3x,D18.9)

      stop
      end
