      program lagrange

c...Polynomial reconstruction in Lagrange form.

      parameter(nmax=300,n1max=10000)
      double precision a(nmax),x(nmax),y(nmax),product,sum
      double precision x2(n1max),y2(n1max),rmax

      open(unit=9,file='lagrange.out')
      open(unit=10,file='interp.dat',status='old')

c...Read points for interpolation
      read(10,*) n
      if(n.gt.nmax) then
        write(9,*) n
        write(9,*) 'Too many interpolation---increase nmax'
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

c...Form the coefficients of the Lagrange reconstruction.
      do 20, i=1,n
        product = 1.
        do 15, j=1,n
          if(j.ne.i) then
            product = product*(x(i)-x(j))
            if(abs(product).lt.1.D-30) then
              write(9,*) 'divide by zero in computation for a: i,j=',i,j
              stop
            endif
          endif
 15     continue
        if(abs(product).lt.1.D-30) then
          write(9,*) 'divide by zero in computation for a:  i=', i
          stop
        endif
        a(i) = y(i)/product
 20   continue

      write(9,*) 'Lagrange coefficients'
      sum = 0.
      do 30, i=1,n
        sum = sum + a(i)
        write(9,400) i,a(i)
 30   continue
      write(9,*) 'Sum of coefficients = ', sum
      write(9,*) '-----------------------------------------------------'
      write(9,*) 'Polynomial Evaluated at Interpolation Points'

c...Validate the reconstruction
      do 60, k = 1,n
        sum = 0.
        do 50, i=1,n      
          product = 1.
          do 40, j=1,n
            if(j.ne.i) then
              product = product*(x(k)-x(j))
            endif
 40       continue
          sum = sum + a(i)*product
 50     continue      
        write(9,500) x(k),y(k),sum-y(k)
 60   continue

      open(unit=11,file='lagrange.plt')
  
      rmax = -1.
      do 90, k = 1,n1
        sum = 0.
        do 80, i=1,n      
          product = 1.
          do 70, j=1,n
            if(j.ne.i) then
              product = product*(x2(k)-x(j))
            endif
 70       continue
          sum = sum + a(i)*product
 80     continue      
        rmax = max(rmax,abs(y2(k)-sum))
        write(11,500) x2(k),sum
 90   continue

      write(9,*) '-----------------------------------------------------'
      write(9,*) 'L-INFINITY ERROR = ', rmax

      close(unit=9)
      close(unit=11)

 500  format(D18.9,3x,D18.9,3x,D18.9)
 400  format(i2,2x,D18.9)

      stop
      end
