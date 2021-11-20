      program spline

c...Cubic Spline Reconstruction

      parameter(nmax=300,n1max=20000)
      double precision a(nmax),b(nmax),c(nmax),d(nmax),m(0:nmax)
      double precision x(0:nmax),y(0:nmax),x2(n1max),y2(n1max),yy
      double precision delta_x, six_delta, delta_x2
      double precision a3(0:nmax),a2(0:nmax),a1(0:nmax),a0(0:nmax)

      open(unit=10,file='interp.dat',status='old')
      open(unit=11,file='spline.plt')
      open(unit=12,file='spline.dat')

c...Read points for interpolation
      read(10,*) n
      if(n.gt.nmax) then
        write(11,*) n
        write(11,*) 'Too many interpolation---increase nmax'
        stop
      endif
      n = n-1
      do 10, i=0,n
        read(10,*) x(i),y(i)
 10   continue
      read(10,*) n1
      if(n1.gt.n1max) then
        write(11,*) n
        write(11,*) 'Too many plotting points---increase n1max'
        stop
      endif
      do 11, i=1,n1
        read(10,*) x2(i),y2(i)
 11   continue
      
      close(unit=10)

      delta_x = (x(n) - x(0))/real(n)
      delta_x2 = delta_x*delta_x
      six_delta = 6.d0/delta_x2

c...Form tridiagonal system of equations
      do 20, i= 1,n-1
        a(i) = 1.d0
        b(i) = 4.d0
        c(i) = 1.d0
        d(i) = six_delta*(y(i+1)-2.d0*y(i)+y(i-1))
 20   continue
      
      call trid(n-1,a,b,c,d,m)

      m(0) = 0.
      m(n) = 0.

      do 50, i=0,n-1
        a3(i) =  (m(i+1)-m(i))/(6.d0*delta_x)
        a2(i) =  .5d0*m(i)
        a1(i) = (y(i+1)-y(i))/delta_x - delta_x*(2.0d0*m(i)+m(i+1))/6.d0
        a0(i) = y(i)
        write(12,*) '[', x(i),',',x(i+1),']:'
        write(12,*) 'a3:  ', a3(i)
        write(12,*) 'a2:  ', a2(i)
        write(12,*) 'a1:  ', a1(i)
        write(12,*) 'a0:  ', a0(i)
        write(12,*)
        write(12,*)
 50   continue

      do 110, j=1,n1
        do 100, i=0,n-1
        if(x2(j).ge.x(i).and.x2(j).le.x(i+1)) then
c...Easy way
          yy = a3(i)*(x2(j)-x(i)) + a2(i)
          yy = yy*(x2(j)-x(i)) + a1(i)
          yy = yy*(x2(j)-x(i)) + a0(i)
c...Hard way
c         yy = m(i+1)*(x2(j)-x(i))*(x2(j)-x(i))*(x2(j)-x(i))
c         yy = yy-m(i)*(x2(j)-x(i+1))*(x2(j)-x(i+1))*(x2(j)-x(i+1))
c         yy = yy+(6.*y(i+1)-m(i+1)*delta_x2)*(x2(j)-x(i))
c         yy = yy-(6.*y(i)-m(i)*delta_x2)*(x2(j)-x(i+1))
c         yy = yy/(6.*delta_x)
        endif
 100    continue
        write(11,500) x2(j),yy
 110  continue

      close(unit=11)
      close(unit=12)

 450  format(D18.9)
 500  format(D18.9,3x,D18.9,3x,D18.9)

      stop
      end

      subroutine trid(n,a,b,c,d,x)
      integer n
      double precision a(n),b(n),c(n),d(n),x(0:n)

c...Zero out the subdiagonal
      do 10, i=2,n     
c       a(i) = a(i) - a(i)*b(i-1)/b(i-1)
        b(i) = b(i) - a(i)*c(i-1)/b(i-1)
        d(i) = d(i) - a(i)*d(i-1)/b(i-1)
 10   continue

c...Solve the upper-triangular bidiagonal system using
c...back-substitution. 
      x(n) = d(n)/b(n)
      do 20, i=n-1,1,-1
        x(i) = (d(i)-c(i)*x(i+1))/b(i)
 20   continue

      return
      end
