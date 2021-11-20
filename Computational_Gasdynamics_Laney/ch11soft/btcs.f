      program btcs
c...Performs backward-time central-space  (IMPLICIT EULER)
      parameter (nmax=100,delta=.000001)
      double precision lambda,u(-2:nmax+2),h(1:nmax),u0(1:nmax+1)
      double precision a(1:nmax), b(1:nmax), c(1:nmax)
      double precision d(1:nmax), e, f

      open(unit=9,file='btcs.out')

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

c...Form ``periodic tridiagonal'' system of equations

c...BTCS
        do 20, i= 1,n
c...Lower sub-diagonal
          a(i) = -.5*lambda
c...Diagonal
          b(i) = 1.0
c...Upper sub-diagonal
          c(i) = .5 * lambda
c...Right hand side 
          d(i) = u(i)
c...Upper right element
          e = -.5* lambda
c...Lower left element
          f = .5 * lambda
 20     continue

c...BTBS
c       do 20, i= 1,n
c...Lower sub-diagonal
c         a(i) = -lambda
c...Diagonal
c         b(i) = 1.0 + lambda
c...Upper sub-diagonal
c         c(i) = 0.
c...Right hand side 
c         d(i) = u(i)
c...Upper right element
c         e = - lambda
c...Lower left element
c         f = 0.
c20     continue

        call ptrid(n,a,b,c,d,e,f,h)

        do 150, i=1,n
          u(i) = h(i)
 150    continue

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
      open(unit=10,file='btcs.plt')
      do 900, i=1,n+1
        write(10,1150) -1 + real(2*i-2)/real(n), u(i),u0(i)
 900  continue
      close(unit=10)

      stop

 1000 write(9,*) '****Error reading data point number ', i,'****'
      close(unit=8)
      close(unit=9)
      stop

 1050 format(4x,'N',11x,'INITIAL',12x,'BTCS',11x, 'DIFFERENCE')
 1100 format(i5,5x,f13.8,5x,f13.8,5x,f13.8)
 1150 format(f15.9,5x,e25.9,5x,f15.9)

      end

      subroutine ptrid(n,a,b,c,d,e,f,x)
      integer n
      double precision a(n),b(n),c(n),d(n),e,f,x(n)
      double precision r(n),s(n)

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
